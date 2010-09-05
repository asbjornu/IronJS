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
    } with
      member x.ResolveVar name =
        x.VarResolver x name
        
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
      | _                             -> Types.JsType.Nothing
        
    //-------------------------------------------------------------------------
    // Resolves the JsType of every variable on target.Scope.Variables
    // and returns a seq<string * int * Set<Ast.Var.Opts> * Types.JsType>
    let private resolveVarTypes (target:Target) =
      let activeVars = ref Set.empty

      let rec resolveVarType (name:string) =
        match target.Scope.TryGetVariable name with
        | None -> 
          match target.Scope.TryGetClosure name with
          | None -> failwith "Que, Global?"
          | Some(_, index, _, _) ->
            let sbType = (target.Closure.GetField(sprintf "Item%i" index).FieldType)
            Types.clrToJs (sbType.GetGenericArguments().[0])

        | Some(var) ->
          let name, index, opts, assignedFrom = var

          if Var.isParameter var then
            if index >= target.ParamCount 
              then Types.JsType.Undefined
              else Types.clrToJs (target.ParamType index)

          elif Set.contains name !activeVars  then Types.JsType.Nothing
          elif assignedFrom.Count = 0         then Types.JsType.Undefined

          else
            activeVars := Set.add name !activeVars

            let type' =               
              assignedFrom
                |>  Seq.map (fun tree -> resolveAstType tree resolveVarType)
                |>  Seq.fold (fun state type' -> type' ||| state) Types.JsType.Nothing
                  
            activeVars := Set.remove name !activeVars
            type'

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
                  then (Dlr.param name (Types.makeStrongBox type')) :> Dlr.Expr
                  else (Dlr.param name type') :> Dlr.Expr

              let proxyExpr = 
                if Var.needsProxy var 
                  then Some((Dlr.param (sprintf "%s_proxy" name) type') :> Dlr.Expr)
                  else None

              name, index, opts, (varExpr, proxyExpr)
            )

        |>  Array.ofSeq
        |>  Seq.ofArray
        
    //-------------------------------------------------------------------------
    // Functions used to filter/extract values out of Variable quads
    let private isLocal var =
      match var with
      | _, -1, _, _ -> true
      | _ -> false

    let private isParameter var = not (isLocal var)
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

      | Identifier(name) -> ctx.ResolveVar name
      | Function(scope, tree) -> compileFunction ctx scope tree

      //
      | Block(trees) -> Dlr.block [for tree in trees -> compileAst ctx tree]
      | Assign(ltree, rtree) -> compileAssign ctx ltree rtree
      | _ -> failwithf "Failed to compile %A" tree

    and private compileFunction ctx scope tree =
      let types =
        scope.Closures
          |> Seq.sortBy (fun v -> Quad.snd v)
          |> Seq.map (fun v -> Quad.fst v)
          |> Seq.map (fun n -> (ctx.ResolveVar n).Type)
          |> Array.ofSeq

      let closureType = 
        Types.createClosureType types

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

      Dlr.blockTmp closureType (fun tmp ->
        let createClosureInitExpr c =
          let field = Dlr.field tmp (sprintf "Item%i" (Quad.snd c))
          Dlr.assign field (ctx.ResolveVar (Quad.fst c))
        
        [Dlr.newArgsT<Types.Function> [tmp; Dlr.constant funCompiler]] 
          |> Seq.append (scope.Closures |> Seq.map createClosureInitExpr)
          |> Seq.append [Dlr.assign tmp (Dlr.new' closureType)]
      )

    and private compileAssign (ctx:Context) ltree rtree =
      let rexpr = compileAst ctx rtree
      let lexpr = 
        match ltree with
        | Identifier(i) -> ctx.ResolveVar i
        | _ -> failwith "Failed to compile %A" ltree

      assign lexpr rexpr

    and private defaultVarResolver (ctx:Context) (name:string) =
      match Seq.tryFind (fun (n, _, _, _) -> n = name) ctx.VarMap with
      | Some(_, _, _, (expr, _)) -> expr
      | None -> 
        match ctx.Target.Scope.TryGetClosure name with
        | None -> failwith "Que?"
        | Some(_, index, _, _) -> (Dlr.field (Dlr.field ctx.Closure (sprintf "Item%i" index)) "Value")

    and private assign lexpr rexpr =
      if Types.isStrongBox lexpr.Type 
        then Dlr.assign (Dlr.field lexpr "Value") rexpr
        else Dlr.assign lexpr rexpr

    //-------------------------------------------------------------------------
    // Main compiler function that setups compilation and invokes compileAst
    and compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
        VarMap = generateVarMap target
        VarResolver = defaultVarResolver
        Closure = Dlr.param "~closure" target.Closure
      }

      //Initialization for closure
      let initClosure =
        [Dlr.assign ctx.Closure (Dlr.cast target.Closure (defaultVarResolver ctx "~closure_proxy"))]

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
          |> Seq.map (fun (expr, proxy) -> assign expr proxy)
          #if DEBUG
          |> Seq.toArray
          #endif

      //Local variables
      let locals =
        ctx.VarMap
          |> Seq.filter isLocal
          |> Seq.map toExpr
          |> Seq.append [ctx.Closure]
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

      let functionBody = 
        (compileAst ctx target.Ast :: [])
          |> Seq.append initClosure
          |> Seq.append initProxied
          |> Seq.append initUndefined
          |> Seq.append initClosedOver
          |> Dlr.blockWithLocals locals

      let lambda = Dlr.lambda target.Delegate parameters functionBody
      lambda.Compile()
      