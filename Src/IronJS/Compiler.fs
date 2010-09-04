namespace IronJS

  module Compiler =
    
    open IronJS
    open IronJS.Aliases
    open IronJS.Ast
    
    (*
    *)
    type Target = {
      Ast: Ast.Tree
      Scope: Ast.Scope
      Delegate: Types.ClrType
    } with
      member x.DelegateTypeArgs = x.Delegate.GetGenericArguments()
      member x.ParamType i = x.DelegateTypeArgs.[i]
      member x.ParamCount = x.DelegateTypeArgs.Length - 1

    type Options = {
      DynamicScopeLevel: int
    }

    type Context = {
      Options: Options
      Target: Target
    }

    type VariableType
      = Local
      | Param of int

    type Variable
      = Expr     of Dlr.Expr
      | Variable of Dlr.ExprParam * VariableType
      | Proxied  of Dlr.ExprParam * Dlr.ExprParam * int

    (*
    *)
    let private generateVarMap (target:Target) (varMap:Map<string, Types.JsType>) =
      target.Scope.Variables
        |>  Seq.map (fun var ->
              let name, index, opts, _ = var
              let type' = Types.jsToClr varMap.[name]
              let varExpr = 
                if Var.isClosedOver opts
                  then  Dlr.param name (Types.makeStrongBox type')
                  else  Dlr.param name type'

              name,
                if Var.isParameter opts then
                    match target.ParamType index <> type', Var.needsProxy opts with
                    | true, _ | _, true ->  
                      let varProxy = Dlr.param (sprintf "%s_proxy" name) type'
                      Proxied(varExpr, varProxy, index)

                    | _ -> Variable(varExpr, Param(index))
                else
                  Variable(varExpr, Local)
            )
        |>  Map.ofSeq

    (*
    *)
    let rec private resolveType tree func =
      match tree with
      | Ast.Identifier(name)          -> func name
      | Ast.Boolean(_)                -> Types.JsType.Boolean
      | Ast.String(_)                 -> Types.JsType.String
      | Ast.Number(_)                 -> Types.JsType.Number
      | Ast.Typed(type', _)           -> type'
      | Ast.Null                      -> Types.JsType.Null
      | Ast.Undefined                 -> Types.JsType.Undefined
      | Ast.Function(_, _)            -> Types.JsType.Function
      | Ast.Unary(op, tree)           -> resolveType tree func
      | Ast.Binary(op, ltree, rtree)  -> resolveType ltree func ||| resolveType rtree func
      | _                             -> Types.JsType.Nothing
        
    (*
    *)
    let private resolveVarTypes (target:Target) =
      let jsTypeMap = ref Map.empty
      let activeVars = ref Set.empty

      let rec resolveVarType (name:string) =
        match Map.tryFind name !jsTypeMap with
        | Some(type') -> Some(type')
        | None ->
          match target.Scope.TryGetVariable name with
          | None -> None
          | Some(v) ->
            let name, index, opts, assignedFrom = v

            if Var.isParameter opts then
              if index >= target.ParamCount 
                then Some(Types.JsType.Undefined)
                else Some(Types.clrToJs (target.ParamType index))

            elif Set.contains name !activeVars  then Some(Types.JsType.Nothing)
            elif assignedFrom.Count = 0         then Some(Types.JsType.Undefined)
            else
              activeVars := Set.add name !activeVars

              let type' =               
                assignedFrom
                  |>  Seq.map   (fun tree -> 
                                  match resolveType tree resolveVarType with
                                  | None -> failwith "Que?"
                                  | Some(type') -> type' )
                  |>  Seq.fold  (fun state type' -> type' ||| state) Types.JsType.Nothing
                  
              activeVars := Set.remove name !activeVars
              Some(type')
  
      target.Scope.Variables
        |>  Seq.iter (fun (name, _, _, _) -> 
              match resolveVarType name with
              | None -> failwith "Que?"
              | Some(type') -> jsTypeMap := Map.add name type' !jsTypeMap
            )

      !jsTypeMap
        |> Map.map (fun _ t -> Types.jsToClr t)

    (*
    *)
    let compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
      }

      resolveVarTypes target

      (*
      resolveVarTypes target
        |>  Map.map (fun name type' -> 
                match ctx.Target.Scope.TryGetVariable name with
                | None -> failwith "Que?"
                | Some(var) -> 

                  let options = 
                    var.Options
                      |>  (fun o -> 
                            if var.IsParameter && var.Index < target.ParamCount 
                                then (o.Remove VarOpts.IsParameter).Add VarOpts.InitToUndefined
                                else o.Remove VarOpts.InitToUndefined
                          )

                      |>  (fun o ->
                            if type' 
                          )

                  let options = 
                    var.Options
                      |>  Seq.map (fun opt -> 
                            match opt with
                            | Ast.VariableOptions.IsParameter -> 
                              if var.Index < target.ParamCount 
                                then [Ast.VariableOptions.InitToUndefined] 
                                else [opt]
                            | _ -> [opt]
                          )
                      |>  Seq.concat
                      |>  Set.ofSeq

                  type', var.Index, options
            )
        *)
      