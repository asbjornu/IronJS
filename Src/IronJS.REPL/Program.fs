open IronJS
open System

IO.Directory.SetCurrentDirectory(@"C:\Users\fredrikhm\Personal\IronJS\Src\IronJS.Ast")

let x = Ast.Parsers.ecma3 (IO.File.ReadAllText("Script.js"))
