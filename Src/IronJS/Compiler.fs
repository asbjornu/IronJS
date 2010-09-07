﻿namespace IronJS

  module Compiler =
    
    open System

    open IronJS
    open IronJS.Aliases
    open IronJS.Utils
    open IronJS.Ast
    
    //-------------------------------------------------------------------------
    type Target = {
      Ast: Ast.Tree
      MetaData: Ast.MetaData
      Delegate: Types.ClrType
    } with
      member x.DelegateTypeArgs = x.Delegate.GetGenericArguments()
      member x.ParamType i = x.DelegateTypeArgs.[i]
      member x.ParamCount = x.DelegateTypeArgs.Length - 1

    type Context = {
      Target: Target
      VarMap: seq<string * int * Set<VariableFlags> * Option<Dlr.Expr>>
      Scope: Dlr.ExprParam
      ReturnLabel: Dlr.Label
    } with
      member x.Environment = Dlr.field x.Closure "Env"
      member x.Globals = Dlr.field x.Environment "Globals"
      member x.ScopeValues = Dlr.field x.Scope "Values"
      member x.ScopeValue i = Dlr.access x.ScopeValues [Dlr.constant i]
      member x.Scopes = Dlr.field x.Closure "Scopes"
      member x.Closure = 
        match x.VarMap |> Seq.find (fun (n,_,_,_) -> n = "~closure") with
        | _, _, _, Some(expr) -> expr
        | _ -> failwith "Que?"
      
    //-------------------------------------------------------------------------
    // Utility functions for compiling IronJS ast trees
    module Utils =

      let identifierIsGlobal (ctx:Context) name =
        match ctx.Target.MetaData.TryGetVariable name with
        | Some(_) -> false
        | None ->
          match ctx.Target.MetaData.TryGetClosure name with
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

        let setType type' (expr:Dlr.Expr) =
          Dlr.assign (typeField expr) (Dlr.constant type')

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
        let setProperty expr name value = Dlr.call expr "Set" [Dlr.constant name; Box.boxValue value]
        let getProperty expr name = Dlr.call expr "Get" [Dlr.constant name]
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
    // Compiler functions
    let rec private compileAst (ctx:Context) tree =
      match tree with
      //Literals
      | Pass -> Dlr.void'
      | Undefined -> Dlr.constant Types.Undefined.Boxed
      | String(s) -> Dlr.constant s
      | Number(n) -> Dlr.constant n
      | Boolean(b) -> Dlr.constant b

      | Return(tree) -> _compileReturn ctx tree
      | New(type', funcTree, initTrees) -> _compileNew ctx type' funcTree initTrees
      | Property(tree, name) -> _compilePropertyAccess ctx tree name
      | Identifier(name) -> _defaultVarResolver ctx name
      | Function(scope, tree) -> _compileFunction ctx scope tree
      | Invoke(callTree, argTrees) -> _compileInvoke ctx callTree argTrees
      | Block(trees) -> Dlr.block [for tree in trees -> compileAst ctx tree]
      | Assign(ltree, rtree) -> _compileAssign ctx ltree rtree
      | Eval(tree)-> _compileEval ctx tree
      | _ -> failwithf "Failed to compile %A" tree
      
    and eval (scope:Types.Scope) (closure:Types.Closure) (source:string) = 
      let tree = Ast.Parsers.Ecma3.parse source
      Types.Undefined.Boxed
      
    //-------------------------------------------------------------------------
    // Compiles a return statement, e.g: return 1;
    and private _compileReturn ctx tree =
      Dlr.return' ctx.ReturnLabel (Utils.Box.boxValue (compileAst ctx tree))
      
    //-------------------------------------------------------------------------
    // Compiles a call to eval, e.g: eval('foo = 1');
    and private _compileEval ctx evalTree =
      let invokeTarget = Dlr.constant (new Func<Types.Scope, Types.Closure, string, Types.Box>(eval))
      let invokeArgs = [ctx.Scope :> Dlr.Expr; ctx.Closure; compileAst ctx evalTree]
      Dlr.invoke invokeTarget invokeArgs
      
    //-------------------------------------------------------------------------
    // Compiles a new operation, e.g: {}, [], new foo();
    and private _compileNew ctx type' ctorTree initTrees =
      match type' with
      | Types.JsType.Object ->

        let initExpressions =
          match initTrees with
          | None -> []
          | _ -> failwith "Que?"
        
        match ctorTree with
        | Some(_) -> failwith "Que?"
        | None -> Dlr.newT<Types.Object>

      | _ -> failwith "Que?"
      
    //-------------------------------------------------------------------------
    // Compiles access to a property, e.g: foo.bar;
    and private _compilePropertyAccess ctx tree name =
      let target = compileAst ctx tree

      if Types.Utils.Object.isObject target.Type 
        then Utils.Object.getProperty target name
        else failwith "Que?"
      
    //-------------------------------------------------------------------------
    // Compiles an invoke statement, foo(arg1, arg2, [arg3, ...]);
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
    // Compiles a function definition, e.g: var foo = function() { ... };
    and private _compileFunction ctx metaData tree =
      let closureType = typeof<Types.Closure>

      let target = {
        Ast = tree
        MetaData = metaData
        Delegate = null
      }

      let funCompiler = 
        new Func<Types.ClrType, Delegate>(
          fun x -> compile {target with Delegate = x}
        )

      let closureExpr = Dlr.newArgsT<Types.Closure> [ctx.Environment; ctx.Scopes; ctx.Scope]
      Dlr.newArgsT<Types.Function> [closureExpr; Dlr.constant funCompiler; Dlr.constant (0,0)]
      
    //-------------------------------------------------------------------------
    // Compiles an assignment operation, e.g: foo = 1; or foo.bar = 1;
    and private _compileAssign (ctx:Context) ltree rtree =
      let rexpr = compileAst ctx rtree

      match ltree with
      | Identifier(name) -> //Variable assignment: foo = 1
        if Utils.identifierIsGlobal ctx name 
          then Utils.Object.setProperty ctx.Globals name rexpr
          else Utils.assign (_defaultVarResolver ctx name) rexpr

      | Property(tree, name) -> //Property assignment: foo.bar = 1
        let target = compileAst ctx tree
        if Types.Utils.Object.isObject target.Type 
          then Utils.Object.setProperty target name rexpr
          else failwith "Que?"

      | _ -> failwithf "Failed to compile assign for: %A" ltree
      
    //-------------------------------------------------------------------------
    and private _defaultVarResolver (ctx:Context) (name:string) =
      match Seq.tryFind (fun (n, _, _, _) -> n = name) ctx.VarMap with
      | Some(_, index, _, _) -> ctx.ScopeValue index
      | None -> 
        match ctx.Target.MetaData.TryGetClosure name with
        | None -> //Global
          Utils.Object.getProperty ctx.Globals name
        | Some(closure) ->
          let fromScope, indexInScope = closure.Indexes
          let scope = Dlr.access ctx.Scopes [Dlr.constant fromScope]
          Dlr.access (Dlr.field scope "Values") [Dlr.constant indexInScope]

    //-------------------------------------------------------------------------
    // Main compiler function that setups compilation and invokes compileAst
    and compile (target:Target) =

      //-------------------------------------------------------------------------
      // Resolves the JsType of every variable on target.Scope.Variables
      // and returns a seq<string * int * Set<Ast.Var.Opts> * Types.JsType>
      let resolveVarTypes (target:Target) =

        let rec resolveVarType activeVars (name:string) =
          match name with
          | "~closure" -> Types.JsType.Closure
          | _ -> 
            match target.MetaData.TryGetVariable name with
            | None -> Types.JsType.Dynamic //Not a local variable
            | Some(var) -> //Normal variable
              match var.StaticType with
              | Some(type') -> type'
              | None -> 
                if Set.contains name activeVars then
                  Types.JsType.Nothing
                else
                  let activeVars' = 
                    Set.add var.Name activeVars

                  let initialState = 
                    if   var.IsForcedDynamic  then Types.JsType.Dynamic
                    elif var.IsParameter      then Types.clrToJs (target.ParamType var.Index)
                    elif var.InitToUndefined  then Types.JsType.Undefined
                    else Types.JsType.Nothing 

                  var.AssignedFrom
                    |> Seq.map (fun tree -> resolveAstType tree (resolveVarType activeVars'))
                    |> Seq.fold (fun state type' -> type' ||| state) initialState

        target.MetaData.Variables
          |> Seq.map (fun var -> var.Name, var.Index, resolveVarType Set.empty var.Name)
          #if DEBUG
          |> Seq.toArray
          #endif
        
      //-------------------------------------------------------------------------
      // Demotes parameters without arguments to normal locals
      let demoteParamsWithoutArguments (var:Ast.Variable) =
        if var.IsParameter then
          if var.Index >= target.ParamCount then 
            let removedParamFlag = var.RemoveFlag VariableFlags.Parameter
            var.SetFlag VariableFlags.InitToUndefined
          else 
            var.RemoveFlag VariableFlags.InitToUndefined
        else
          var

      //-------------------------------------------------------------------------
      // Generates a variable map of type list<name * Variable> 
      let generateVarMap (target:Target) =
        target.MetaData.Variables
          |>  Seq.map demoteParamsWithoutArguments
          |>  Seq.map demoteParamsWithoutArguments
          |>  Seq.map (fun var ->
                let name, index, opts, type' = var
                let type' = Types.jsToClr type'

                let varExpr = 
                  if Var.isParameter var
                    then Some(Dlr.param name type' :> Dlr.Expr)
                    else None

                name, index, opts, varExpr
              )

          |>  Array.ofSeq
        
      //-------------------------------------------------------------------------
      // Functions used to filter/extract values out of Variable quads
      let isLocal var =
        match var with
        | _, _, _, None -> true
        | _ -> false

      let toExpr var = 
        match var with
        | _, _, _, Some(expr) -> expr
        | _ -> failwith "Que?"

      let toIndex (_, index, _, _) = index
      let hasOpt opt (_, _, o, _) = Set.contains opt o
      let notPrivate (name:string, _, _, _) = name.[0] <> '~'
          
      //-------------------------------------------------------------------------
      // Main Context
      let ctx = {
        Options = options
        Target = target
        VarMap = generateVarMap target
        VarResolver = _defaultVarResolver
        Scope = Dlr.paramT<Types.Scope> "~scope"
        ReturnLabel = Dlr.labelT<Types.Box> "~return"
      }

      //Initlization for Scope object
      let initScope = 
        let args = [Dlr.constant ctx.Target.Scope.VarCount]
        let newScope = Dlr.newArgsT<Types.Scope> args
        Dlr.assign ctx.Scope newScope

      //Initilization for variables that need to be set to undefined
      let initUndefined =
        ctx.VarMap
          |> Seq.filter (hasOpt Var.InitToUndefined)
          |> Seq.map (fun (_, index, _, _)->
                        (Utils.Box.setType 
                          (Types.JsType.Undefined)
                          (ctx.ScopeValue index)
                        )
                     )
          #if DEBUG
          |> Seq.toArray
          #endif
      
      //Parameter variables
      let parameters =
        ctx.VarMap
          |> Seq.filter (hasOpt Var.IsParameter)
          |> Seq.sortBy toIndex
          |> Seq.map toExpr
          |> Seq.cast<Dlr.ExprParam>
          #if DEBUG
          |> Seq.toArray
          #endif

      //Main function body
      let functionBody = 
        [Dlr.labelExprT<Types.Box> ctx.ReturnLabel]
          |> Seq.append [compileAst ctx target.Ast]
          |> Seq.append initUndefined
          |> Seq.append initForEval
          |> Seq.append [initScope]
          |> Dlr.blockWithLocals [ctx.Scope]

      let lambda = Dlr.lambda target.Delegate parameters functionBody

      #if INTERACTIVE
      Dlr.Utils.printDebugView lambda
      #endif

      lambda.Compile()
      