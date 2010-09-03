namespace IronJS

  module Ast = 

    type Scope = {
      Locals: string Set
    } with
      static member New parms = {
        Locals = Set.ofSeq parms
      }

    module Tree =

      type BinaryOp 
        //Math
        = Add
        | Sub

      type UnaryOp 
        = Inc
        | Dec

      type Node
        //Constants
        = String  of string
        | Number  of double
        | Boolean of bool
        | Pass
        | Null
        | Undefined

        //Ops
        | Binary  of BinaryOp * Node * Node
        | Unary   of UnaryOp  * Node

        //
        | Assign      of Node * Node
        | Block       of Node list
        | Identifier  of string * (int * int)
        | Var         of Node
        | Return      of Node
        | With        of Node * Node
        | Function    of string option * string list * Node

      let walk func tree = 
        match tree with
        | Identifier(_)
        | Boolean(_)
        | String(_)
        | Number(_)
        | Pass
        | Null
        | Undefined -> 
          tree

        | Binary(op, ltree, rtree) -> Binary(op, func ltree, func rtree)
        | Unary(op, tree) -> Unary(op, func tree)
        
        | Assign(ltree, rtree) -> Assign(func rtree, (func ltree))
        | Block(trees) -> Block([for tree in trees -> func tree])
        | Var(tree) -> Var(func tree)
        | Return(tree) -> Return(func tree)
        | With(target, tree) -> With(target, tree)
        | Function(name, parms, tree) -> Function(name, parms, func tree)

    module Filters =
        
      let stripVariableDefinitions trees = 

        let collectVars tree =
          let vars = ref Set.empty

          let rec walker tree =
            match tree with
            | Tree.Var(Tree.Assign(Tree.Identifier(n, r), rtree)) ->
              vars := Set.add n !vars
              Tree.Assign(Tree.Identifier(n, r), Tree.walk walker rtree)

            | Tree.Var(Tree.Identifier(n, _)) -> 
              vars := Set.add n !vars
              Tree.Pass

            | tree -> Tree.walk walker tree

          walker tree, !vars

        Map.map (fun _ (tree, scope) -> 
          
          let tree, vars = collectVars tree
          tree, {scope with Locals = Set.union scope.Locals vars}

        ) trees

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
        | ES3Parser.Identifier      -> Tree.Identifier(text tok, (-1, -1))
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


