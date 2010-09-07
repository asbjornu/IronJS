namespace IronJS

  module Utils =

    open IronJS.Aliases

    let getCtor (typ:System.Type) (args:System.Type list) =
      Array.find (fun (ctor:CtorInfo) ->
        let parms = List.ofArray (ctor.GetParameters())
        if args.Length = parms.Length 
          then Seq.forall2 (fun a (p:ParmInfo) -> p.ParameterType.IsAssignableFrom a) args parms
          else false
      ) (typ.GetConstructors())

    module Quad =
      let fst (first, _, _, _)  = first
      let snd (_, second, _, _) = second
      let trd (_, _, third, _)  = third
      let fth (_, _, _, fourth) = fourth

    module Tri =
      let fst (first, _, _)   = first
      let snd (_, second, _)  = second
      let trd (_, _, third)   = third
