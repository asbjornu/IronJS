namespace IronJS

  module Compiler =
    
    open IronJS
    open IronJS.Aliases
    
    (*
    *)
    type Target = {
      Ast: Ast.Node
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
    let generateVariableMap (target:Target) =

      let createVarExpr (var:Ast.Variable) =
        if var.IsClosedOver 
          then  Dlr.param var.Name (Types.jsToClr var.Type)
          else  Dlr.param var.Name (Types.makeStrongBox (Types.jsToClr var.Type))
          
      let createVarProxy (var:Ast.Variable) =
        Dlr.param (sprintf "%s_proxy" var.Name) (target.ParamType var.Index)

      target.Scope.Variables
        |>  Seq.map (fun v ->
              if v.IsParameter then
                v.Name,
                  match target.ParamType v.Index <> Types.jsToClr v.Type, v.NeedsProxy with
                  | true, true
                  | true, false 
                  | false, true -> Proxied(createVarExpr v, createVarProxy v, v.Index)
                  | _ -> Variable(createVarExpr v, Param(v.Index))
              else
                v.Name, 
                  Variable(createVarExpr v, Local)
            )
        |>  Map.ofSeq
        
    (*
    *)
    let resolveVarTypes (target:Target) =
      let activeVariables = ref Set.empty
      let resolvedVariables = ref Map.empty

      let rec resolveVarType (var:Ast.Variable) =
        match var.Type with
        | Types.JsType.Nothing ->
          if var.AssignedFrom.Count = 0 then
            Some(Types.JsType.Dynamic)
          else
            let type'  =
              var.AssignedFrom
                |>  Seq.map   (fun tree -> 
                                match evaluateNodeType tree with
                                | None -> failwith "Que?"
                                | Some(type') -> type' )
                |>  Seq.fold  (fun state type' -> type' ||| state) Types.JsType.Nothing

            Some(type')
        | _ -> Some(var.Type)

      and evaluateNodeType tree =
        Ast.evaluateType tree (Some(fun name -> 
          if Set.contains name !activeVariables then
            Some(Types.JsType.Nothing)
          else
            activeVariables := Set.add name !activeVariables
            match target.Scope.TryGetVariable name with
            | Some(var) -> 
              let type' = resolveVarType var
              activeVariables := Set.remove name !activeVariables
              type'

            | None -> failwith "Que?"
        ))

      target.Scope.Variables
        |>  Seq.map (fun v -> v.Name, resolveVarType v)
        |>  Map.ofSeq

    (*
    *)
    let compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
      }

      resolveVarTypes target
      