﻿open IronJS
open IronJS.Ast
open IronJS.Compiler
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree  = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let tree' = applyDefaultFilters 1 tree

let compiled = 
  match tree' with
  | Ast.Function(metaData, ast) -> 
    let target = {
      Ast = ast
      MetaData = metaData
      Delegate = Types.Utils.createDelegateType [typeof<Types.Closure>; typeof<Types.Box>]
    }

    Compiler.compile target
  | _ -> failwith "Que?"

let env = new Types.Environment()
let closure = new Types.Closure(env)
compiled.DynamicInvoke(closure)