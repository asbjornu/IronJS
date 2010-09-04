open IronJS
open IronJS.Compiler
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let stripped = Ast.stripVarStatements tree
let levels = Ast.analyzeScopeLevels stripped
let closures = Ast.analyzeClosureScopes levels
let assign = Ast.analyzeAssignment closures

let compiled = 
  match assign with
  | Ast.Function(scope, ast) -> 

    let target = {
      Ast = ast
      Scope = scope
      Delegate = typeof<Func<Types.Closure, Types.Box>>
    }

    let options = {
      DynamicScopeLevel = -1
    }

    Compiler.compile target options


  | _ -> failwith "Que?"