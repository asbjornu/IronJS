namespace IronJS

  module Compiler =
    
    open System

    open IronJS
    open IronJS.Aliases
    open IronJS.Utils
    open IronJS.Ast
    
    //-------------------------------------------------------------------------
    type Target = {
      Ast: Ast.Tree
      Scope: Ast.Scope
      Delegate: Types.ClrType
      Closure: Types.ClrType
    } with
      member x.ClosureTypeArgs = x.Closure.GetGenericArguments()
      member x.DelegateTypeArgs = x.Delegate.GetGenericArguments()
      member x.ParamType i = x.DelegateTypeArgs.[i]
      member x.ParamCount = x.DelegateTypeArgs.Length - 1

    type Options = {
      DynamicScopeLevel: int
    }

    type Context = {
      Options: Options
      Target: Target
      VarMap: seq<string * int * Set<Var.Opts> * (Dlr.Expr * Option<Dlr.Expr>)>
      VarResolver: Context -> string -> Dlr.Expr
      Closure: Dlr.Expr
      ReturnLabel: Dlr.Label
      EvalScope: Dlr.Expr
    } with
      member x.ResolveVar name = x.VarResolver x name
      member x.Environment = Dlr.field x.Closure "Env"
      member x.Globals = Dlr.field x.Environment "Globals"
      
    //-------------------------------------------------------------------------
    // Utility functions for compiling IronJS ast trees
    module Utils =

      let identifierIsGlobal (ctx:Context) name =
        match ctx.Target.Scope.TryGetVariable name with
        | Some(_) -> false
        | None ->
          match ctx.Target.Scope.TryGetClosure name with
          | Some(_) -> false
          | None -> true

      module Box = 
        let typeToField expr type' = Dlr.field expr (Types.Utils.Box.typeToFieldName type')
        let typeField expr = Dlr.field expr "Type"
        let clrField expr = Dlr.field expr "Clr"

        let boxValue (value:Dlr.Expr) = 
          if Types.Utils.Box.isBox value.Type then value
          else
            let type' = Types.clrToJs value.Type
            Dlr.blockTmpT<Types.Box> (fun tmp ->
              [
                (Dlr.assign (typeField tmp) (Dlr.constant type'))
                (Dlr.assign (typeToField tmp type') value)
                (tmp :> Dlr.Expr)
              ] |> Seq.ofList
            )

        let typeCheck expr (type':Types.JsType) true' false' =
          (Dlr.ternary 
            (Dlr.eq (typeField expr) (Dlr.constant type'))
            (true')
            (false')
          )

        let unBox type' (expr:Dlr.Expr) =
          if Types.Utils.Box.isBox expr.Type 
            then Dlr.cast type' (clrField expr)
            else failwith "Que?"

        let unBoxT<'a> expr = unBox typeof<'a> expr

        let assignValue (lexpr:Dlr.Expr) (rexpr:Dlr.Expr) =
          let type' = Types.clrToJs rexpr.Type

          if Types.Utils.Box.isBox rexpr.Type then
            Dlr.assign lexpr rexpr
          else
            (Dlr.block
              [
                (Dlr.assign (typeField lexpr) (Dlr.constant type'))
                (Dlr.assign (typeToField lexpr type') rexpr)
              ]
            )
      
      module Object =
        let setProperty expr (name:string) value = Dlr.call expr "Set" [Dlr.constant name; Box.boxValue value]
        let getProperty expr (name:string) = Dlr.call expr "Get" [Dlr.constant name]
        let unBox expr = Box.unBoxT<Types.Object> expr

      module Function =
        let closureExpr (expr:Dlr.Expr) = Dlr.field expr "Closure"
        let unBox expr = Box.unBoxT<Types.Function> expr

      let assign (lexpr:Dlr.Expr) rexpr =
        let lexpr = 
          if Types.Utils.StrongBox.isStrongBox lexpr.Type
            then Dlr.field lexpr "Value"
            else lexpr

        if Types.Utils.Box.isBox lexpr.Type 
          then Box.assignValue lexpr rexpr
          else Dlr.assign lexpr rexpr
        
    //-------------------------------------------------------------------------
    // Resolves the type of an Ast.Tree object, invoking 'func'
    // to resolve the type of Ast.Identifier objects
    let rec private resolveAstType tree func =
      match tree with
      | Ast.Identifier(name)          -> func name
      | Ast.Boolean(_)                -> Types.JsType.Boolean
      | Ast.String(_)                 -> Types.JsType.String
      | Ast.Number(_)                 -> Types.JsType.Number
      | Ast.Typed(type', _)           -> type'
      | Ast.Null                      -> Types.JsType.Null
      | Ast.Undefined                 -> Types.JsType.Undefined
      | Ast.Function(_, _)            -> Types.JsType.Function
      | Ast.Unary(op, tree)           -> resolveAstType tree func
      | Ast.Binary(op, ltree, rtree)  -> resolveAstType ltree func ||| resolveAstType rtree func
      | Ast.New(type', _, _)          -> type'
      | Ast.Property(_, _)            -> Types.JsType.Dynamic
      | _                             -> Types.JsType.Nothing
        
    //-------------------------------------------------------------------------
    // Resolves the JsType of every variable on target.Scope.Variables
    // and returns a seq<string * int * Set<Ast.Var.Opts> * Types.JsType>
    let private resolveVarTypes (target:Target) =
      let activeVars = ref Set.empty

      let rec resolveVarType (name:string) =
        match name with
        | "~closure_proxy" -> Types.JsType.Closure
        | _ -> 
          match target.Scope.TryGetVariable name with
          | None -> //Not a normal variable, try closure
            match target.Scope.TryGetClosure name with
            | None -> Types.JsType.Dynamic //Not a closure either, must be global
            | Some(_, index, _, _) -> //Closure variable
              let strongBoxType = target.Closure.GetField(sprintf "Item%i" index).FieldType
              Types.clrToJs (strongBoxType.GetGenericArguments().[0])

          | Some(var) -> //Normal variable, 
            let name, index, opts, assignedFrom = var

            //This mutable hack deals with undefined params
            let paramType = ref Types.JsType.Nothing
            if index >= target.ParamCount then paramType := Types.JsType.Undefined
            
            //Evaluate type of property
            if Var.forceDynamic var             then Types.JsType.Dynamic
            elif Var.isParameter var            then Types.clrToJs (target.ParamType index)
            elif Set.contains name !activeVars  then Types.JsType.Nothing
            elif assignedFrom.Count = 0         then Types.JsType.Undefined
            else
              activeVars := Set.add name !activeVars
              let type' =               
                assignedFrom
                  |>  Seq.map (fun tree -> resolveAstType tree resolveVarType)
                  |>  Seq.fold (fun state type' -> type' ||| state) Types.JsType.Nothing
              activeVars := Set.remove name !activeVars
              type' ||| !paramType

      target.Scope.Variables
        |> Seq.map (fun (n, i, o, _) -> n, i, o, resolveVarType n)
        #if DEBUG
        |> Seq.toArray
        #endif
        
    //-------------------------------------------------------------------------
    // Normalizes all Ast.Var.Opts values for every variable in varSeq
    let private normalizeVarOptions (target:Target) varSeq =

      let removeParamsWithoutArgValues (name, index, opts:Set<_>, type') =
        let index, opts = 
          if Set.contains Var.IsParameter opts && index >= target.ParamCount 
            then -1, Set.add Var.InitToUndefined (Set.remove Var.IsParameter opts)
            else index, Set.remove Var.InitToUndefined opts
        name, index, opts, type'

      let resolveProxyNeed (name, index, opts:Set<_>, type') =
        let opts =
          if Set.contains Var.IsParameter opts then
            if Set.contains Var.IsClosedOver opts then Set.add Var.NeedsProxy opts
            elif type' <> (Types.clrToJs (target.ParamType index)) then Set.add Var.NeedsProxy opts
            else opts
          else
            opts
        name, index, opts, type'

      varSeq
        |> Seq.map removeParamsWithoutArgValues
        |> Seq.map resolveProxyNeed
        #if DEBUG
        |> Seq.toArray
        #endif
        
    //-------------------------------------------------------------------------
    // Generates a variable map of type list<name * Variable> 
    let private generateVarMap (target:Target) =
      let varTypes = resolveVarTypes target
      let varMap = normalizeVarOptions target varTypes

      varMap
        |>  Seq.map (fun var ->
              let name, index, opts, type' = var
              let type' = Types.jsToClr type'

              let varExpr = 
                if Var.isClosedOver var
                  then (Dlr.param name (Types.Utils.StrongBox.make type')) :> Dlr.Expr
                  else (Dlr.param name type') :> Dlr.Expr

              let proxyExpr = 
                if Var.needsProxy var 
                  then Some((Dlr.param (sprintf "%s_proxy" name) (target.ParamType index)) :> Dlr.Expr)
                  else None

              name, index, opts, (varExpr, proxyExpr)
            )

        |>  Array.ofSeq
        |>  Seq.ofArray
        
    //-------------------------------------------------------------------------
    // Functions used to filter/extract values out of Variable quads
    let private isLocal var =
      match var with
      | _, _, _, (_, Some(_))
      | _, -1, _, _ -> true
      | _ -> false

    let private isParameter var = 
      match var with
      | _, -1, _, _ -> false
      | _ -> true

    let private isProxied var =
      match var with
      | _, _, _, (_, Some(_)) -> true
      | _ -> false

    let private isVar opt (_, _, o, _) = Set.contains opt o
    let private isClosedOver = isVar Var.Opts.IsClosedOver
    let private isInitUndefined =  isVar Var.Opts.InitToUndefined
    let private toExpr (_, _, _, (expr, _)) = expr

    let private toBothExprs var =
      match var with
      | _, _, _, (exp, Some(proxy)) -> exp, proxy
      | _ -> failwith "Que?"

    let private toParameter var =
      match var with
      | _, _, _, (expr, None)
      | _, _, _, (_, Some(expr)) -> expr
      
    //-------------------------------------------------------------------------
    // Compiler functions
    let rec private compileAst (ctx:Context) tree =
      match tree with
      //Literals
      | String(s) -> Dlr.constant s
      | Number(n) -> Dlr.constant n
      | Boolean(b) -> Dlr.constant b

      | New(type', funcTree, initTrees) -> _compileNew ctx type' funcTree initTrees
      | Property(tree, name) -> _compileProperty ctx tree name
      | Identifier(name) -> ctx.ResolveVar name
      | Function(scope, tree) -> _compileFunction ctx scope tree
      | Invoke(callTree, argTrees) -> _compileInvoke ctx callTree argTrees
      | Block(trees) -> Dlr.block [for tree in trees -> compileAst ctx tree]
      | Assign(ltree, rtree) -> _compileAssign ctx ltree rtree
      | Eval(tree) -> _compileEval ctx tree
      | _ -> failwithf "Failed to compile %A" tree
      
    and eval (vars:CDict<string, Types.StrongBox<Types.Box>>) (closure:Types.Closure) (source:string) = 
      let tree = Ast.Parsers.Ecma3.parse source
      Types.Undefined.Boxed

    and private _compileEval ctx tree =
      Dlr.void'
      (*
      let evalDelegate = new Func<array<string * Types.StrongBox<Types.Box>>, Types.Closure, string, Types.Box>(eval)

      let closures =
        ctx.Target.Scope.Closures
          |>  Seq.map (fun (n, i, _, _) -> 
                Dlr.newArgsT<string * Types.StrongBox<Types.Box>> [
                  (Dlr.constant n)
                  (Dlr.field ctx.Closure (sprintf "Item%i" i))
                ]
              )

      let variables =
        ctx.Target.Scope.Variables
          |>  Seq.filter (fun (n, _, _, _) -> n.[0] <> '~')
          |>  Seq.map (fun (n, i, _, _) -> 
                Dlr.newArgsT<string * Types.StrongBox<Types.Box>> [
                  (Dlr.constant n)
                  (ctx.ResolveVar n)
                ]
              )

      let vars = 
        Dlr.newArrayItemsT<string * Types.StrongBox<Types.Box>> (Seq.append closures variables)

      Dlr.invoke (Dlr.constant evalDelegate) [vars; ctx.Closure; compileAst ctx tree]
      *)


    and private _compileNew ctx type' ftree itrees =
      match type' with
      | Types.JsType.Object ->

        let initExprs =
          match itrees with
          | None -> []
          | _ -> failwith "Que?"
        
        match ftree with
        | Some(_) -> failwith "Que?"
        | None -> Dlr.newT<Types.Object>

      | _ -> failwith "Que?"

    and private _compileProperty ctx tree name =
      let target = compileAst ctx tree
      Utils.Object.getProperty target name
      
    //-------------------------------------------------------------------------
    // Compiles an invoke statement, foo(arg1, arg2, [arg3, ...])
    and private _compileInvoke ctx ctree atrees =
      let callTarg = compileAst ctx ctree
      let callArgs = [for tree in atrees -> compileAst ctx tree]

      let buildFuncType callArgs =
        Types.Utils.createDelegateType (
          List.foldBack 
            (fun (itm:Dlr.Expr) state -> itm.Type :: state) 
            (callArgs)
            [typeof<Types.Box>]
        )

      let buildFuncCall callTarg =
        Dlr.blockTmpT<Types.Function> (fun tmp ->
          let callArgs = Utils.Function.closureExpr tmp :: callArgs
          let funcType = buildFuncType callArgs
          let compiled = Dlr.callGeneric tmp "CompileAs" [funcType] []
          [
            (Dlr.assign tmp callTarg)
            (Dlr.invoke compiled callArgs)
          ] |> Seq.ofList
        )

      if Types.Utils.Function.isFunction callTarg.Type 
        then buildFuncCall callTarg
        else
          Dlr.blockTmpT<Types.Box> (fun tmp ->
            [
              (Dlr.assign tmp callTarg)
              (Utils.Box.typeCheck
                (tmp)
                (Types.JsType.Function)
                (buildFuncCall (Utils.Function.unBox tmp))
                (Dlr.constant Types.Undefined.Boxed)
              )
            ] |> Seq.ofList
          )
        
    //-------------------------------------------------------------------------
    // Compiles a function definition (not the function itself), function(...) { ... }
    and private _compileFunction ctx scope tree =
      let types =
        scope.Closures
          |> Seq.sortBy (fun v -> Quad.snd v)
          |> Seq.map (fun v -> Quad.fst v)
          |> Seq.map (fun n -> (ctx.ResolveVar n).Type)
          |> Array.ofSeq

      let closureType = Types.Utils.Closure.createType types
      let target = {
        Ast = tree
        Scope = scope
        Delegate = null
        Closure = closureType
      }

      let funCompiler = 
        new Func<Types.ClrType, Delegate>(
          fun x -> compile {target with Delegate = x} {DynamicScopeLevel = -1}
        )

      let funKey = int64 (tree.GetHashCode()), closureType.TypeHandle.Value

      Dlr.blockTmp closureType (fun tmp ->
        let createClosureInitExpr c =
          let field = Dlr.field tmp (sprintf "Item%i" (Quad.snd c))
          Dlr.assign field (ctx.ResolveVar (Quad.fst c))
        
        [Dlr.newArgsT<Types.Function> [tmp; Dlr.constant funCompiler; Dlr.constant funKey]] 
          |> Seq.append (scope.Closures |> Seq.map createClosureInitExpr)
          |> Seq.append [Dlr.assign (Dlr.field tmp "Env") (ctx.Environment)]
          |> Seq.append [Dlr.assign tmp (Dlr.new' closureType)]
      )
      
    //-------------------------------------------------------------------------
    // Compiles an assignment operatiom
    and private _compileAssign (ctx:Context) ltree rtree =
      let rexpr = compileAst ctx rtree

      match ltree with
      | Identifier(name) -> //Variable assignment, foo = 1
        if Utils.identifierIsGlobal ctx name 
          then Utils.Object.setProperty ctx.Globals name rexpr
          else Utils.assign (ctx.ResolveVar name) rexpr

      | Property(tree, name) -> //Property assignment, foo.bar = 1
        let target = compileAst ctx tree
        if Types.Utils.Object.isObject target.Type 
          then Utils.Object.setProperty target name rexpr
          else failwith "Que?"

      | _ -> failwithf "Failed to compile assign for: %A" ltree

    and private _defaultVarResolver (ctx:Context) (name:string) =
      match Seq.tryFind (fun (n, _, _, _) -> n = name) ctx.VarMap with
      | Some(_, _, _, (expr, _)) -> expr
      | None -> //Not a normal variable
        match ctx.Target.Scope.TryGetClosure name with
        | None -> //Global
          Utils.Object.getProperty ctx.Globals name
        | Some(_, index, _, _) -> //Closure
          Dlr.field (Dlr.field ctx.Closure (sprintf "Item%i" index)) "Value"

    //-------------------------------------------------------------------------
    // Main compiler function that setups compilation and invokes compileAst
    and compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
        VarMap = generateVarMap target
        VarResolver = _defaultVarResolver
        Closure = Dlr.param "~closure" target.Closure
        EvalScope = Dlr.param "~evalScope" typeof<CDict<string, Types.StrongBox<Types.Box>>>
        ReturnLabel = Dlr.labelT<Types.Box> "~return"
      }

      //Initialization for closure
      let initClosure =
        [Dlr.assign ctx.Closure (Dlr.cast target.Closure (ctx.ResolveVar "~closure_proxy"))]

      //Initialization for closed over variables
      let initClosedOver =
        ctx.VarMap
          |> Seq.filter isClosedOver
          |> Seq.map toExpr
          |> Seq.map (fun expr -> Dlr.assign expr (Dlr.new' expr.Type))
          #if DEBUG
          |> Seq.toArray
          #endif

      //Initialization for undefined variables
      let initUndefined =
        ctx.VarMap
          |> Seq.filter isInitUndefined
          |> Seq.map toExpr
          |> Seq.map (fun expr -> Dlr.assign expr (Types.Undefined.InstanceExpr))
          #if DEBUG
          |> Seq.toArray
          #endif

      //Intialization for proxied parameters
      let initProxied =
        ctx.VarMap
          |> Seq.filter isProxied
          |> Seq.map toBothExprs
          |> Seq.map (fun (expr, proxy) -> Utils.assign expr proxy)
          #if DEBUG
          |> Seq.toArray
          #endif

      //Local variables
      let locals =
        ctx.VarMap
          |> Seq.filter isLocal
          |> Seq.map toExpr
          |> Seq.append [ctx.Closure; ctx.EvalScope]
          |> Seq.cast<Dlr.ExprParam>
          #if DEBUG
          |> Seq.toArray
          #endif

      //Parameter variables
      let parameters =
        ctx.VarMap
          |> Seq.filter isParameter
          |> Seq.sortBy (fun quad -> Quad.snd quad)
          |> Seq.map toParameter
          |> Seq.cast<Dlr.ExprParam>
          #if DEBUG
          |> Seq.toArray
          #endif

      //Initilization for scopes with eval inside them
      let initEvalScope =
        if Set.contains ScopeOpts.HasEval target.Scope.Options 
          then 
            ctx.VarMap
              |> Seq.filter isLocal
              |> Seq.map (fun (name, _, _, (expr, _)) -> name, expr)
              |> Seq.map (fun (name, expr) -> Dlr.call ctx.EvalScope "Add" [Dlr.constant name; expr])
              |> Seq.append [Dlr.assign ctx.EvalScope (Dlr.new' ctx.EvalScope.Type)]
              #if DEBUG
              |> Seq.toArray
              #endif

          else [Dlr.void'] 
              |> Seq.ofList
              #if DEBUG
              |> Seq.toArray
              #endif

      let functionBody = 
        [Dlr.labelExprT<Types.Box> ctx.ReturnLabel]
          |> Seq.append [compileAst ctx target.Ast]
          |> Seq.append initEvalScope
          |> Seq.append initProxied
          |> Seq.append initUndefined
          |> Seq.append initClosedOver
          |> Seq.append initClosure
          |> Dlr.blockWithLocals locals

      let lambda = Dlr.lambda target.Delegate parameters functionBody

      Dlr.Utils.printDebugView lambda

      lambda.Compile()
      