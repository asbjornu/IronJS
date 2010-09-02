namespace IronJS

  module Cst = 

    open IronJS
    open System.Collections.Generic

    type Tree
      //Constants
      = String  of string
      | Number  of double
      | Boolean of bool
      | Pass
      | Null
      | Undefined

      //Variables
      | Identifier of string * (int * int)

      //Ops
      | Binary  of IronJS.Type * Ast.BinaryOp * Tree * Tree
      | Unary   of IronJS.Type * Ast.UnaryOp  * Tree

      //
      | Assign   of IronJS.Type * Tree * Tree
      | Block    of Tree list
      | Return   of Tree
      | With     of Tree
      | Function of int64

    type Scope = {
      Locals : string Set
      SyntaxTree : Tree
    } with
      static member New = {
        Locals = Set.empty
        SyntaxTree = Pass
      }

    let convertAstToCst ast = 

      let locals = new List<HashSet<string>>()
      let ctrees = new List<Tree>()

      let rec convert (vars:HashSet<string>) ast =
        match ast with
        //Constants
        | Ast.String(s)   -> Tree.String(s)
        | Ast.Number(n)   -> Tree.Number(n)
        | Ast.Boolean(b)  -> Tree.Boolean(b)
        | Ast.Null        -> Tree.Null
        | Ast.Undefined   -> Tree.Undefined

        //Variables
        | Ast.Identifier(i) -> Tree.Identifier(i, (-1, -1))
        | Ast.Var(t) ->
          match t with
          | Ast.Assign(Ast.Identifier(i), rt) ->
            vars.Add(i) |> ignore
            Tree.Assign(Type.Nothing, Tree.Identifier(i, (-1, -1)), convert vars rt)

          | Ast.Identifier(i) ->
            vars.Add(i) |> ignore
            Tree.Pass

          | _ -> failwith "Ast.Var can only contain Ast.Assign(Ast.Identifier(_), _) or Ast.Identifier(_)"

        //Ops
        | Ast.Binary(op, t1, t2)  -> Tree.Binary(Type.Nothing, op, convert vars t1, convert vars t2)
        | Ast.Unary(op, t)        -> Tree.Unary(Type.Nothing, op, convert vars t)
      
        //
        | Ast.Assign(t1, t2)    -> Tree.Assign(Type.Nothing, convert vars t1, convert vars t2)
        | Ast.Block(l)          -> Tree.Block([for x in l -> convert vars x])
        | Ast.Return(t)         -> Tree.Return(convert vars t)
        | Ast.With(t)           -> Tree.With(convert vars t)
        | Ast.Function(_, p, b) -> 
          let set = new HashSet<string>(p)

          ctrees.Add(convert set b)
          locals.Add(set)

          Tree.Function(int64 (ctrees.Count))

        //| _ -> failwith "Failed"

      let set = new HashSet<string>()

      ctrees.Add(convert set ast)
      locals.Add(set)

      (List.ofSeq ctrees, List.ofSeq (seq {for x in locals -> Set.ofSeq x}))
      

