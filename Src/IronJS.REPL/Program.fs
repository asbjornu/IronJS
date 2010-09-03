
open IronJS
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.ecma3 (IO.File.ReadAllText("Script.js"))
let trees = Ast.Tree.split tree 10L
let filters = [Ast.Filters.collectVars]

//List.fold (fun s f -> f s) trees filters

Ast.Filters.collectVars (fst trees.[11L]) |> ignore