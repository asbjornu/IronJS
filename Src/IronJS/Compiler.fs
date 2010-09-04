namespace IronJS

  module Compiler =
    
    open IronJS
    open IronJS.Aliases

    type Target = {
      Ast: Ast.Node
      Scope: Ast.Scope
      Delegate: Types.ClrType
    } with
      member x.DelegateTypeArgs = x.Delegate.GetGenericArguments()
      member x.GetParamType i = x.DelegateTypeArgs.[i+1]

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
      | Proxied  of Dlr.ExprParam * Dlr.ExprParam  * int

    let generateVariableMap (target:Target) =
      let createVarExpr (var:Ast.Variable) =
        if var.IsClosedOver 
          then  Dlr.param var.Name (Types.jsToClr var.Type)
          else  Dlr.param var.Name (Types.makeStrongBox (Types.jsToClr var.Type))
          
      let createVarProxy (var:Ast.Variable) =
        Dlr.param (sprintf "%s_proxy" var.Name) (target.GetParamType var.Index)

      target.Scope.Variables
        |> Seq.map (fun v ->
          match v.IsParameter with
          | true  ->
            
            
          | false ->
            Variable(create v, Local)

        )


    let compile (target:Target) (options:Options) =
      let ctx = {
        Options = options
        Target = target
      }

      let locals = 
        ctx.Target.Scope.Variables
          |> Seq.filter (fun x -> not x.IsParameter)
          |> Seq.map (fun x -> x.
      

      let types = target.DelegateType.GetGenericArguments()

      ()
      