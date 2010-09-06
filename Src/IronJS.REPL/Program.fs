
open IronJS
open IronJS.Ast
//open IronJS.Compiler
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let filters = 
  [
    Ast.stripVarDeclarations
    Ast.detectEval
    Ast.analyzeClosureScopes
    Ast.analyzeAssignment
  ]
let tree' = List.fold (fun t f -> f t) tree filters

(*
let compiled = 
  match tree' with
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
let closure = new Types.Closure(env)
compiled.DynamicInvoke(closure);
*)
