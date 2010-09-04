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
    let analyzeVariableTypes (target:Target) =
      target.DelegateTypeArgs

    (*
    *)
    let compile (target:Target) (options:Options) =

      let ctx = {
        Options = options
        Target = target
      }

      let types = target.DelegateType.GetGenericArguments()

      ()
      