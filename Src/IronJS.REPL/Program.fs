open IronJS
open IronJS.Ast
open IronJS.Compiler
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let filters = 
  [
    Ast.stripVarDeclarations 1
    Ast.detectEval
    Ast.analyzeClosureScopes
    Ast.analyzeAssignment
    Ast.analyzeStaticTypes
  ]
let tree' = List.fold (fun t f -> f t) tree filters

let compiled = 
  match tree' with
  | Ast.Function(metaData, ast) -> 
    let target = {
      Ast = ast
      MetaData = metaData
      Delegate = Types.Utils.createDelegateType [typeof<Types.Closure>; typeof<Types.Box>]
      IndexOffset = 0
    }

    Compiler.compile target
  | _ -> failwith "Que?"

let env = new Types.Environment()
let closure = new Types.Closure(env)
compiled.DynamicInvoke(closure);
