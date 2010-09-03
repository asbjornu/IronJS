open IronJS
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.Ecma3.parse (IO.File.ReadAllText("Script.js"))
let stripped = Ast.stripVarStatements tree
let levels = Ast.analyzeScopeLevels stripped
let closures = Ast.analyzeClosureScopes levels
let assign = Ast.analyzeAssignment closures