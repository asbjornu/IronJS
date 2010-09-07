#light
#r @"FSharp.PowerPack"
#r @"../Lib/Antlr3.Runtime.dll"
#r @"../Lib/Xebic.ES3.dll"
#r @"../Lib/Microsoft.Dynamic.dll"

#load "Aliases.fs"
#load "Utils.fs"
#load "Dlr.fs"
#load "Types.fs"
#load "Ast.fs"
#load "Compiler.fs"

open IronJS
open IronJS.Ast
open IronJS.Compiler
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let filters = 
  [
    Ast.stripVarDeclarations
    Ast.detectEval
    Ast.analyzeClosureScopes
    Ast.analyzeAssignment
    Ast.analyzeStaticTypes
  ]
let tree' = List.fold (fun t f -> f t) (Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))) filters

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
compiled.DynamicInvoke(closure);

env.Globals.Get("x").Double