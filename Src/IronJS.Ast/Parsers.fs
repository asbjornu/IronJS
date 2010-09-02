namespace IronJS.Ast

module Parsers =

  open IronJS
  open Xebic.ES3

  (*Utility Alias*)
  type private AntlrToken = Antlr.Runtime.Tree.CommonTree

  (*Utility Methods*)
  let private children (tok:AntlrToken) = 
    if tok.Children = null then []
    else
      tok.Children
        |> Seq.cast<AntlrToken> 
        |> Seq.toList

  let private cast (tok:obj) = 
    tok :?> AntlrToken

  let private hasChild (tok:AntlrToken) index =
    tok.ChildCount > index

  let private child (tok:AntlrToken) index = 
    if hasChild tok index then cast tok.Children.[index] else null

  let private text (tok:AntlrToken) = 
    tok.Text

  let private jsString (tok:AntlrToken) = 
    let str = text tok
    str.Substring(1, str.Length - 2)

  (*Translator*)
  let rec private translate<'a> (tok:AntlrToken) =
    match tok.Type with
    | 0 
    | ES3Parser.BLOCK           -> Ast.Node.Block(Type.Nothing, [for x in (children tok) -> translate x])
    | ES3Parser.VAR             -> Ast.Node.Var(Type.Nothing, translate (child tok 0))
    | ES3Parser.ASSIGN          -> Ast.Node.Assign(Type.Nothing, translate (child tok 0), translate (child tok 1))
    | ES3Parser.Identifier      -> Ast.Node.Identifier(Type.Nothing, text tok)
    | ES3Parser.StringLiteral   -> Ast.Node.String(Type.Nothing, jsString tok)
    | ES3Parser.DecimalLiteral  -> Ast.Node.Number(Type.Nothing, double (text tok))

    | _ -> failwithf "No parser for token %s (%i)" (ES3Parser.tokenNames.[tok.Type]) tok.Type
  
  (*Parser*)
  let rec ecma3<'a> str = 
    let lexer = new Xebic.ES3.ES3Lexer(new Antlr.Runtime.ANTLRStringStream(str))
    let parser = new Xebic.ES3.ES3Parser(new Antlr.Runtime.CommonTokenStream(lexer))
    translate<'a> (parser.program().Tree :?> AntlrToken)


