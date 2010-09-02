#light
#r @"FSharp.PowerPack"
#r @"../Lib/Antlr3.Runtime.dll"
#r @"../Lib/Xebic.ES3.dll"
#r @"../Lib/Microsoft.Dynamic.dll"

#load "Type.fs"
#load "Node.fs"
#load "Parsers.fs"

open IronJS
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS.Ast")

let x = Ast.Parsers.ecma3 (IO.File.ReadAllText("Script.js"))

