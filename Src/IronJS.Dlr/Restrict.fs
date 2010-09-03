namespace IronJS.Dlr

  (*
  *)
  module Restrict =

    type BR = System.Dynamic.BindingRestrictions
  
    let notAtAll = BR.Empty
    let byExpr expr = BR.GetExpressionRestriction(expr)
    let byType expr typ = BR.GetTypeRestriction(expr, typ)
    let byInstance expr instance = BR.GetInstanceRestriction(expr, instance)

    let argRestrict (a:System.Dynamic.DynamicMetaObject) =
      let restriction = 
        if a.HasValue && a.Value = null 
          then BR.GetInstanceRestriction(a.Expression, Expr.null')
          else BR.GetTypeRestriction(a.Expression, a.LimitType)

      a.Restrictions.Merge(restriction)

    let byArgs (args:System.Dynamic.DynamicMetaObject seq) =
      Seq.fold (fun (s:BR) a -> s.Merge(argRestrict a)) BR.Empty args