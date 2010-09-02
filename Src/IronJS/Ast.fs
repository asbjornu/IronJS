namespace IronJS.Ast

  type BinaryOp 
    //Math
    = Add
    | Sub

  type UnaryOp 
    = Inc
    | Dec

  type Tree
    //Constants
    = String  of string
    | Number  of double
    | Boolean of bool
    | Null
    | Undefined

    //Ops
    | Binary  of BinaryOp * Tree * Tree
    | Unary   of UnaryOp  * Tree

    //
    | Assign      of Tree * Tree
    | Block       of Tree list
    | Identifier  of string
    | Var         of Tree
    | Return      of Tree
    | With        of Tree
    | Function    of string option * string list * Tree

  module Parsers =

    open IronJS
    open Xebic.ES3

    (*Utility Alias*)
    type private AntlrToken = 
      Antlr.Runtime.Tree.CommonTree

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
    let rec private translate (tok:AntlrToken) =
      match tok.Type with
      | 0 
      | ES3Parser.BLOCK           -> Tree.Block([for x in (children tok) -> translate x])
      | ES3Parser.VAR             -> Tree.Var(translate (child tok 0))
      | ES3Parser.ASSIGN          -> Tree.Assign(translate (child tok 0), translate (child tok 1))
      | ES3Parser.Identifier      -> Tree.Identifier(text tok)
      | ES3Parser.StringLiteral   -> Tree.String(jsString tok)
      | ES3Parser.DecimalLiteral  -> Tree.Number(double (text tok))

      | ES3Parser.FUNCTION -> 
        Tree.Function(None, [for x in (children (child tok 0)) -> text x], translate (child tok 1))

      | _ -> failwithf "No parser for token %s (%i)" (ES3Parser.tokenNames.[tok.Type]) tok.Type
  
    (*Parser*)
    let ecma3 str = 
      let lexer = new Xebic.ES3.ES3Lexer(new Antlr.Runtime.ANTLRStringStream(str))
      let parser = new Xebic.ES3.ES3Parser(new Antlr.Runtime.CommonTokenStream(lexer))
      translate (parser.program().Tree :?> AntlrToken)


