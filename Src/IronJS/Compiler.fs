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
      MetaData: Ast.MetaData
      Delegate: Types.ClrType
      IndexOffset: int
    } with
      member x.ParamTypes = 
        Dlr.ArrayUtils.RemoveLast(
          Dlr.ArrayUtils.RemoveFirst(
            x.Delegate.GetGenericArguments()))
      member x.ParamType i = x.ParamTypes.[i]
      member x.ParamCount = x.ParamTypes.Length

    type Context = {
      Target: Target
      Scope: Dlr.ExprParam
      Closure: Dlr.ExprParam
      ReturnLabel: Dlr.Label
    } with
      member x.Environment = Dlr.field x.Closure "Env"
      member x.Globals = Dlr.field x.Environment "Globals"
      member x.ScopeValues = Dlr.field x.Scope "Values"
      member x.ScopeValue i = Dlr.access x.ScopeValues [Dlr.constant i]
      member x.ParentScopes = Dlr.field x.Closure "Scopes"
      
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
      | Identifier(name) -> _identifierResolver ctx name
      | Function(scope, tree) -> _compileFunction ctx scope tree
      | Invoke(callTree, argTrees) -> _compileInvoke ctx callTree argTrees
      | Block(trees) -> Dlr.block [for tree in trees -> compileAst ctx tree]
      | Assign(ltree, rtree) -> _compileAssign ctx ltree rtree
      | Eval(tree)-> _compileEval ctx tree
      | _ -> failwithf "Failed to compile %A" tree
      
    and eval (scope:Types.Scope) (closure:Types.Closure) (source:string) = 
      let tree = Ast.Parsers.Ecma3.parse source
      let filters = 
        [
          Ast.stripVarDeclarations 0;
          Ast.detectEval;
          Ast.analyzeClosureScopes;
          Ast.analyzeAssignment;
          Ast.analyzeStaticTypes
        ]
      let analyzed = List.fold (fun t f -> f t) tree filters
      match analyzed with
      | Function(metaData, tree) ->
        let compileTarget = {
          Ast = tree
          MetaData = metaData.SetFlag (MetaDataFlags.IsEval)
          Delegate = typeof<Func<Types.Closure, Types.Scope, Types.Box>>
          IndexOffset = scope.Values.Length
        }

        let compiled = compile compileTarget

        ()

      | _ -> failwith "Que?"

      Types.Undefined.Boxed
      
    //-------------------------------------------------------------------------
    // Compiles a return statement, e.g: return 1;
    and private _compileReturn ctx tree =
      Dlr.return' ctx.ReturnLabel (Utils.Box.boxValue (compileAst ctx tree))
      
    //-------------------------------------------------------------------------
    // Compiles a call to eval, e.g: eval('foo = 1');
    and private _compileEval ctx evalTree =
      let invokeTarget = Dlr.constant (new Func<Types.Scope, Types.Closure, string, Types.Box>(eval))
      let invokeArgs = [ctx.Scope :> Dlr.Expr; ctx.Closure :> Dlr.Expr; compileAst ctx evalTree]
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
      let target = {
        Ast = tree
        MetaData = metaData
        Delegate = null
        IndexOffset = 0
      }

      let funCompiler = 
        new Func<Types.ClrType, Delegate>(
          fun delegateType -> compile {target with Delegate = delegateType}
        )

      let closureArgs = 
        //if target.MetaData.Flags.HasFlag(MetaDataFlags.RequiresParentScope) 
          (*then*) [ctx.Environment; ctx.ParentScopes; ctx.Scope :> Dlr.Expr]
          //else [ctx.Environment; ctx.ParentScopes]

      let closureExpr = Dlr.newArgsT<Types.Closure> closureArgs
      Dlr.newArgsT<Types.Function> [closureExpr; Dlr.constant funCompiler]
      
    //-------------------------------------------------------------------------
    // Compiles an assignment operation, e.g: foo = 1; or foo.bar = 1;
    and private _compileAssign (ctx:Context) ltree rtree =
      let rexpr = compileAst ctx rtree

      match ltree with
      | Identifier(name) -> //Variable assignment: foo = 1
        if Utils.identifierIsGlobal ctx name 
          then Utils.Object.setProperty ctx.Globals name rexpr
          else Utils.assign (_identifierResolver ctx name) rexpr

      | Property(tree, name) -> //Property assignment: foo.bar = 1
        let target = compileAst ctx tree
        if Types.Utils.Object.isObject target.Type 
          then Utils.Object.setProperty target name rexpr
          else failwith "Que?"

      | _ -> failwithf "Failed to compile assign for: %A" ltree
      
    //-------------------------------------------------------------------------
    and private _identifierResolver (ctx:Context) (name:string) =
      match ctx.Target.MetaData.TryGetVariable name with
      | Some(var) -> ctx.ScopeValue (var.Index + ctx.Target.IndexOffset)
      | None -> 
        match ctx.Target.MetaData.TryGetClosure name with
        | None -> //Global
          Utils.Object.getProperty ctx.Globals name
        | Some(closure) ->
          let fromScope, indexInScope = closure.Indexes
          let scope = Dlr.access ctx.ParentScopes [Dlr.constant fromScope]
          Dlr.access (Dlr.field scope "Values") [Dlr.constant indexInScope]

    //-------------------------------------------------------------------------
    // Generates a variable map of type list<name * Variable> 
    and resolveVariableTypes (target:Target) =
      //-------------------------------------------------------------------------
      let rec resolveVarType activeVars name =
        match target.MetaData.TryGetVariable name with
        | None -> Types.JsType.Dynamic //Not a local variable
        | Some(var) -> //Normal variable
          match var.StaticType with
          | Some(type') -> type'
          | None -> 
            if Set.contains name activeVars then
              Types.JsType.Nothing
            else
              let activeVars' = Set.add var.Name activeVars
              let initialState = 
                if   var.IsForcedDynamic  then Types.JsType.Dynamic
                elif var.InitToUndefined  then Types.JsType.Undefined
                elif var.IsParameter      then Types.clrToJs (target.ParamType var.Index)
                else Types.JsType.Nothing 

              var.AssignedFrom
                |> Seq.map (fun tree -> resolveAstType tree (resolveVarType activeVars'))
                |> Seq.fold (fun state type' -> type' ||| state) initialState
        
      //-------------------------------------------------------------------------
      let demoteParamsWithoutArguments (var:Ast.Variable) =
        if var.IsParameter then
          if var.Index >= target.ParamCount then 
            let removedParamFlag = var.RemoveFlag VariableFlags.Parameter
            var.SetFlag VariableFlags.InitToUndefined
          else 
            var.RemoveFlag VariableFlags.InitToUndefined
        else
          var

      {target with 
        MetaData =
          {target.MetaData with
            Variables = 
              target.MetaData.Variables
                |> Seq.map demoteParamsWithoutArguments
                |> Seq.map (fun var -> {var with StaticType = Some(resolveVarType Set.empty var.Name)})
                |> Set.ofSeq
          }
      }

    //-------------------------------------------------------------------------
    // Main compiler function that setups compilation and invokes compileAst
    and compile (target:Target) =
        
      //-------------------------------------------------------------------------
      // Functions used to filter/extract values out of Variables
      let toExpr var = 
        match var with
        | _, _, _, Some(expr) -> expr
        | _ -> failwith "Que?"

      let isLocal (var:Variable) = not var.IsParameter
      let getIndex (var:Variable) = var.Index
      let hasFlag flag (var:Variable) = var.Flags.HasFlag(flag)
      let notPrivate (var:Variable) = var.Name.[0] <> '~'
          
      //-------------------------------------------------------------------------
      // Main Context
      let ctx = {
        Target = resolveVariableTypes target
        Scope = Dlr.paramT<Types.Scope> "~scope"
        Closure = Dlr.paramT<Types.Closure> "~closure"
        ReturnLabel = Dlr.labelT<Types.Box> "~return"
      }

      //Initlization for Scope object
      let initScope = 
        if ctx.Target.MetaData.IsEval then
          Dlr.blockTmpT<Types.Box array> (fun tmp ->
            [
              Dlr.assign tmp (Dlr.Expr.NewArrayBounds(typeof<Types.Box>, Dlr.constant (ctx.Target.MetaData.VariableCount + ctx.Target.IndexOffset)))
              Dlr.callStaticT<System.Array> "Copy" [Dlr.field ctx.Scope "Values"; tmp :> Dlr.Expr; Dlr.constant ctx.Target.IndexOffset]
              Dlr.assign (Dlr.field ctx.Scope "Values") tmp
            ] |> Seq.ofList
          )
        else
          let varIndexMap = Dlr.constant ctx.Target.MetaData.VariableIndexMap
          let varCount = Dlr.constant ctx.Target.MetaData.VariableCount
          let newScope = Dlr.newArgsT<Types.Scope> [varCount; varIndexMap]
          Dlr.assign ctx.Scope newScope

      //Initilization for variables that need to be set to undefined
      let initUndefined =
        ctx.Target.MetaData.Variables
          |> Seq.filter (hasFlag VariableFlags.InitToUndefined)
          |> Seq.map (fun var->
                        (Utils.Box.setType 
                          (Types.JsType.Undefined)
                          (_identifierResolver ctx var.Name)
                        )
                     )
          #if DEBUG
          |> Seq.toArray
          #endif
      
      //Parameter variables
      let parameterExprs =
        ctx.Target.ParamTypes
          |> Seq.mapi (fun i type' -> Dlr.param (sprintf "param%i" i) type')
          |> Seq.toArray

      //Copy parameter values into Scope.Value array
      let copyParameters =
        ctx.Target.MetaData.Variables
          |> Set.filter (hasFlag VariableFlags.Parameter)
          |> Seq.map  (fun var ->
                        (Utils.assign  
                          (_identifierResolver ctx var.Name)
                          (parameterExprs.[var.Index])
                        )
                      )

      //Main function body
      let functionBody = 
        [Dlr.labelExprT<Types.Box> ctx.ReturnLabel]
          |> Seq.append [compileAst ctx target.Ast]
          |> Seq.append initUndefined
          |> Seq.append copyParameters
          |> Seq.append [initScope]
          |> Dlr.blockWithLocals [ctx.Scope]

      let lambda = Dlr.lambda target.Delegate (Seq.append [ctx.Closure] parameterExprs) functionBody

      #if INTERACTIVE
      Dlr.Utils.printDebugView lambda
      #endif

      lambda.Compile()
      