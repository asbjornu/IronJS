namespace IronJS

  module Compiler =
    
    open IronJS
    open IronJS.Aliases
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

    type VariableType
      = Local
      | Param of int

    type Variable
      = Expr     of Dlr.Expr
      | Variable of Dlr.Expr * VariableType
      | Proxied  of Dlr.Expr * Dlr.Expr * int

    type Options = {
      DynamicScopeLevel: int
    }

    type Context = {
      Options: Options
      Target: Target
      VarMap: seq<string * Variable>
      VarResolver: Context -> string -> Dlr.Expr
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
        | None -> failwith "Que?"
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
                |>  Seq.map   (fun tree -> resolveAstType tree resolveVarType)
                |>  Seq.fold  (fun state type' -> type' ||| state) Types.JsType.Nothing
                  
            activeVars := Set.remove name !activeVars
            type'

      target.Scope.Variables
        |>  Seq.map (fun (n, i, o, _) -> n, i, o, resolveVarType n)
        #if DEBUG
        |>  Seq.toArray
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
        |>  Seq.toArray
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
                  then  Dlr.param name (Types.makeStrongBox type')
                  else  Dlr.param name type'

              let varExprs = 
                if Var.isParameter var then
                    if Var.needsProxy var then
                      let varProxy = Dlr.param (sprintf "%s_proxy" name) type'
                      Proxied(varExpr, varProxy, index)
                    else
                      Variable(varExpr, Param(index))
                else
                  Variable(varExpr, Local)

              name, varExprs
            )

        |>  List.ofSeq
        
    //-------------------------------------------------------------------------
    // Functions used to filter/extract values out of Variable unions
    let private isLocal (_, var:Variable) =
      match var with
      | Variable(_, Local) -> true
      | Proxied(_, _, _) -> true
      | _ -> false

    let private toLocal (_, var:Variable) =
      match var with
      | Variable(l, Local) -> l
      | Proxied(l, _, _)  -> l
      | _ -> failwith "Que?"

    let private isParameter (_, var:Variable) =
      match var with
      | Variable(_, Param(_)) -> true
      | Proxied(_, _, _) -> true
      | _ -> false

    let private toParameter (_, var:Variable) =
      match var with
      | Variable(p, Param(i)) -> p, i
      | Proxied(_, p, i)  -> p, i
      | _ -> failwith "Que?"

    let private isProxied (_, var:Variable) =
      match var with
      | Proxied(_, _, _) -> true
      | _ -> false

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
      Dlr.defaultT<Types.Function>

    and private compileAssign (ctx:Context) ltree rtree =
      let rexpr = compileAst ctx rtree
      let lexpr = 
        match ltree with
        | Identifier(i) -> ctx.ResolveVar i
        | _ -> failwith "Failed to compile %A" ltree

      Dlr.assign lexpr rexpr

    and private defaultVarResolver (ctx:Context) (name:string) =
      match Seq.find (fun (n, _) -> n = name) ctx.VarMap with
      | _, Variable(expr, _)
      | _, Proxied(expr, _, _) 
      | _, Expr(expr) -> expr

    //-------------------------------------------------------------------------
    //Main compiler function that setups compilation and invokes compileAst
    and compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
        VarMap = generateVarMap target
        VarResolver = defaultVarResolver
      }

      let locals =
        ctx.VarMap
          |> Seq.filter isLocal
          |> Seq.map toLocal
          |> Seq.cast<Dlr.ExprParam>

      let parameters =
        ctx.VarMap
          |> Seq.filter isParameter
          |> Seq.map toParameter
          |> Seq.sortBy (fun pair -> snd pair)
          |> Seq.map (fun pair -> fst pair)
          |> Seq.cast<Dlr.ExprParam>
          #if DEBUG
          |> Seq.toArray
          #endif

      let functionBody = Dlr.blockWithLocals locals [compileAst ctx target.Ast]
      Dlr.lambda target.Delegate parameters functionBody
      