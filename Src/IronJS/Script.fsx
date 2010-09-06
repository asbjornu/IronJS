﻿#light
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

let tree = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let stripped = Ast.stripVarDeclarations tree
let eval = Ast.analyzeEval stripped
let levels = Ast.analyzeScopeLevels eval
let closures = Ast.analyzeClosureScopes levels
let assign = Ast.analyzeAssignment closures

let compiled = 
  match assign with
  | Ast.Function(scope, ast) -> 
    let target = {
      Ast = ast
      Scope = scope
      Delegate = Types.Utils.createDelegateType [typeof<Types.Closure>; typeof<Types.Box>]
      Closure = typeof<Types.Closure>
    }
    let options = { DynamicScopeLevel = -1 }
    Compiler.compile target options
  | _ -> failwith "Que?"

let env = new Types.Environment()
let closure = new Types.Closure()
closure.Env <- env
compiled.DynamicInvoke(closure)