#light
#r @"FSharp.PowerPack"
#r @"../Lib/Antlr3.Runtime.dll"
#r @"../Lib/Xebic.ES3.dll"
#r @"../Lib/Microsoft.Dynamic.dll"

#load "Type.fs"
#load "Ast.fs"

open IronJS
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS")

let tree = Ast.Parsers.ecma3 (IO.File.ReadAllText("Script.js"))
let trees = Ast.splitTree tree 10L
let filters = [Ast.Filters.collectVars]

List.fold (fun s f -> f s) trees filters
