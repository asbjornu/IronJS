namespace IronJS

  module Aliases = 
    open System

    type CtorInfo = Reflection.ConstructorInfo
    type ParmInfo = Reflection.ParameterInfo
    type FieldInfo = Reflection.FieldInfo
    type MethodInfo = Reflection.MethodInfo
    type CDict<'a, 'b> = Collections.Generic.Dictionary<'a, 'b>

  module Constants =
    let closureParameterName = "~closure"
    let thisParameterName = "~this"
