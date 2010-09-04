namespace IronJS

  module Compiler =
    
    open IronJS
    open IronJS.Aliases
    
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
        |>  Seq.map (fun v ->
              let type' = Types.jsToClr varMap.[v.Name]

              let varExpr = 
                if v.IsClosedOver 
                  then  Dlr.param v.Name (Types.makeStrongBox type')
                  else  Dlr.param v.Name type'

              v.Name,
                if v.IsParameter then
                    match target.ParamType v.Index <> type', v.NeedsProxy with
                    | true, _ | _, true ->  
                      let varProxy = Dlr.param (sprintf "%s_proxy" v.Name) type'
                      Proxied(varExpr, varProxy, v.Index)

                    | _ -> Variable(varExpr, Param(v.Index))
                else
                  Variable(varExpr, Local)
            )
        |>  Map.ofSeq

    (*
    *)
    let rec private resolveType tree func =
      match tree with
      | Ast.Boolean(_) -> Some(Types.JsType.Boolean)
      | Ast.String(_) -> Some(Types.JsType.String)
      | Ast.Number(_) -> Some(Types.JsType.Number)
      | Ast.Typed(type', _) -> Some(type')
      | Ast.Null -> Some(Types.JsType.Null)
      | Ast.Undefined -> Some(Types.JsType.Undefined)
      | Ast.Function(_, _, _) -> Some(Types.JsType.Function)
      
      | Ast.Unary(op, tree) -> resolveType tree func
      | Ast.Binary(op, ltree, rtree) -> 
        match resolveType ltree func, resolveType rtree func with
        | None, _ | _, None -> failwith "Que?"
        | Some(ltype), Some(rtype) -> Some(ltype ||| rtype)
      
      | Ast.Identifier(name) -> func name
      | _ -> None
        
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
          | Some(var) ->
            if var.IsParameter then
              if var.Index >= target.ParamCount then
                failwith "Que?"
              else
                Some(Types.clrToJs (target.ParamType var.Index))

            elif Set.contains name !activeVars then
              Some(Types.JsType.Nothing)

            elif var.AssignedFrom.Count = 0 then
              Some(Types.JsType.Undefined)

            else
              activeVars := Set.add name !activeVars

              let type' =               
                var.AssignedFrom
                  |>  Seq.map   (fun tree -> 
                                  match resolveType tree resolveVarType with
                                  | None -> failwith "Que?"
                                  | Some(type') -> type' )
                  |>  Seq.fold  (fun state type' -> type' ||| state) Types.JsType.Nothing
                  
              activeVars := Set.remove name !activeVars
              Some(type')
  
      target.Scope.Variables
        |>  Seq.iter (fun v -> 
              match resolveVarType v.Name with
              | None -> failwith "Que?"
              | Some(type') -> jsTypeMap := Map.add v.Name type' !jsTypeMap
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

      let varTypeMap = resolveVarTypes target
      ()
      