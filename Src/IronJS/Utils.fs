namespace IronJS

  module Utils =

    module Quad =
      let fst (first, _, _, _)  = first
      let snd (_, second, _, _) = second
      let trd (_, _, third, _)  = third
      let fth (_, _, _, fourth) = fourth

    module Tri =
      let fst (first, _, _)   = first
      let snd (_, second, _)  = second
      let trd (_, _, third)   = third

