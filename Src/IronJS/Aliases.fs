namespace IronJS

  module Aliases = 
    type CtorInfo = System.Reflection.ConstructorInfo
    type ParmInfo = System.Reflection.ParameterInfo
    type FieldInfo = System.Reflection.FieldInfo
    type MethodInfo = System.Reflection.MethodInfo

  module Constants =
    let closureParameterName = "~closure"
    let thisParameterName = "~this"
