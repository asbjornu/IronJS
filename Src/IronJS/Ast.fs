﻿namespace IronJS

  module Ast = 

    open IronJS

    (*
    *)
    type BinaryOp 
      //Math
      = Add = 1
      | Sub = 2
      | Eq = 100
      | NotEq = 101
      | Lt = 102
      | LtEq = 103
      | Gt = 104
      | GtEq = 105
      
    (*
    *)
    type UnaryOp 
      = Inc
      | Dec
      | PostInc
      | PostDec
      | Void

    module Var = 

      type Opts 
        = IsParameter
        | NeedsProxy
        | IsClosedOver
        | InitToUndefined

      let name (n, _, _, _) = n
      let index (_, i, _, _) = i
      let options (_, _, o, _) = o
      let assignedFrom (_, _, _, a) = a
      let isClosedOver (o:Set<Opts>) = o.Contains Opts.IsClosedOver
      let needsProxy (o:Set<Opts>) = o.Contains Opts.NeedsProxy
      let isParameter (o:Set<Opts>) = o.Contains Opts.IsParameter
      let initToUndefined (o:Set<Opts>) = o.Contains Opts.InitToUndefined

    (*
    module Closure =
      let name (n, _, _, _) = n
      let index (_, i, _, _) = i
      let from (_, _, f, _) = f
      let dsl (_, _, _, d) = d
    *)
        
    (*
    *)
    [<StructuralEquality; StructuralComparison>]
    type Tree
      //Constants
      = String  of string
      | Number  of double
      | Boolean of bool
      | Pass
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
      | With        of Tree * Tree
      | Function    of Scope * Tree
      | Typed       of Types.JsType * Tree
      
    (*
    *)
    and [<CustomEquality; CustomComparison>] Scope = {
      Variables: Set<string * int * Set<Var.Opts> * Set<Tree>>
      Closures: Set<string * int * int * int>
      DynamicScopeLevel: int
    } with

      interface System.IComparable with
        member x.CompareTo y =
          match y with
          | :? Scope as y -> compare x y
          | _ -> failwith "Que?"

      override x.Equals (obj) =
        match obj with
        | :? Scope as scope -> false
        | _ -> false

      override x.GetHashCode () =
        x.Variables.GetHashCode()

      member x.UpdateVariable name func = 
        match x.TryGetVariable name with
        | None -> failwith "Que?"
        | Some(v) -> {x with Variables = x.Variables.Remove(v).Add(func v)}
        
      member x.AddVariable var = 
        {x with Variables = x.Variables.Add(var)}
        
      member x.AddClosure closure = 
        {x with Closures = x.Closures.Add(closure)}
        
      member x.TryGetVariable name = 
        Seq.tryFind (fun (n, _, _, _) -> n = name) x.Variables
        
      member x.TryGetClosure name =
        Seq.tryFind (fun (n, _, _, _) -> n = name) x.Closures
        
      static member New parms = {
        Closures = set []
        Variables = parms |> Seq.mapi (fun i x -> x, i, set [Var.Opts.IsParameter], set []) |> Set.ofSeq
        DynamicScopeLevel = -1
      }
        
    (*
    *)
    let private doInsideScope scopes scope func =
      scopes     := scope :: !scopes
      let result  = func()
      let scope   = List.head !scopes
      scopes     := List.tail !scopes
      scope, result
        
    let private currentScope scopes =
      List.head !scopes
        
    (*
    *)
    let walk func tree = 
      match tree with
      | Identifier(_)
      | Boolean(_)
      | String(_)
      | Number(_)
      | Typed(_, _)
      | Pass
      | Null
      | Undefined -> tree

      | Binary(op, ltree, rtree) -> Binary(op, func ltree, func rtree)
      | Unary(op, tree) -> Unary(op, func tree)
        
      | Assign(ltree, rtree) -> Assign(func ltree, (func rtree))
      | Block(trees) -> Block([for tree in trees -> func tree])
      | Var(tree) -> Var(func tree)
      | Return(tree) -> Return(func tree)
      | With(target, tree) -> With(func target, func tree)
      | Function( scope, tree) -> Function(scope, func tree) 

    (*
    *)
    let analyzeAssignment tree =
      let scopes = ref List.empty

      let rec analyze tree =
        match tree with
        | Assign(Identifier(name), rtree) ->
          let scope:Scope = currentScope scopes

          //Update scopes
          match scope.TryGetVariable name with
          | None ->  ()
          | _ ->  

            let tree = 
              match rtree with
              | Function(_, _) -> Typed(Types.JsType.Function, Pass)
              | _ -> rtree

            scopes := scope.UpdateVariable name (fun (n, i, o, a) -> n, i, o, a.Add tree) :: List.tail !scopes

          //Return node
          Assign(Identifier(name), analyze rtree)
          
        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(scope, tree)

        | _ -> walk analyze tree

      analyze tree

    (*
    *)
    let analyzeScopeLevels tree =
      let scopes = ref List.empty
      let dynamicScopeLevel = ref -1

      let rec analyze tree =
        match tree with
        | With(target, tree) -> 
          let target = analyze target

          dynamicScopeLevel := !dynamicScopeLevel + 1
          let tree = analyze tree
          dynamicScopeLevel := !dynamicScopeLevel - 1

          With(target, tree)
            
        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes {scope with DynamicScopeLevel = !dynamicScopeLevel} (fun () -> analyze tree)
          Function(scope, tree)

        | tree -> walk analyze tree

      analyze tree
      
    (*
    *)
    let stripVarStatements tree =
      let scopes = ref List.empty

      let updateTopScope name =
        if (!scopes).Length > 0 then 
          let scope:Scope = List.head !scopes
          scopes := scope.AddVariable (name, -1, set[], set[]) :: List.tail !scopes

      let rec strip tree =
        match tree with
        | Var(Assign(Identifier(name), rtree)) ->
          updateTopScope name
          Assign(Identifier(name), strip rtree)

        | Var(Identifier(name)) ->
          updateTopScope name
          Pass

        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> strip tree)
          Function(scope, tree)

        | tree -> walk strip tree

      strip tree

    (*
    *)
    let analyzeClosureScopes tree =
      let scopes = ref List.empty

      let rec analyze tree =
        match tree with
        | Identifier(name) -> 

          let scope:Scope = currentScope scopes

          match scope.TryGetVariable name with
          | None  -> 
            match scope.TryGetClosure name with
            | None  -> 
              //Build new scope objects with closure variables set
              let _, newScopes = 
                List.foldBack (fun (itm:Scope) (level, scopes) ->
                  match level with
                  | None  ->

                    match itm.TryGetVariable name with
                    | None              ->  None, itm :: scopes
                    | Some(_, _, o, _)  -> 
                      let scope = 
                        if Var.isClosedOver o
                          then itm
                          else itm.UpdateVariable name (fun (n, i, o, a) -> n, i, o.Add Var.Opts.IsClosedOver, a)
                          
                      Some(scopes.Length, itm.DynamicScopeLevel), scope :: scopes

                  | Some(fromScope, dynamicScopeLevel)  -> 

                    let scope = 
                      match itm.TryGetClosure name with
                      | Some(_) ->  itm
                      | None    ->  itm.AddClosure (name, itm.Closures.Count, fromScope, dynamicScopeLevel)

                    level, scope :: scopes

                ) !scopes (None, [])

              //Store updated scopes
              scopes := newScopes

              //Return tree
              tree

            | Some(_) ->  tree
          | Some(_)   ->  tree

        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(scope, tree)

        | tree -> walk analyze tree

      analyze tree
          

    module Parsers =

      (*
      *)
      module Ecma3 = 
        open IronJS
        open Xebic.ES3

        type private AntlrToken = 
          Antlr.Runtime.Tree.CommonTree

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

        (*
        Translator
        *)
        let rec private translate (tok:AntlrToken) =
          match tok.Type with
          | 0 
          | ES3Parser.BLOCK           -> Block([for x in (children tok) -> translate x])
          | ES3Parser.VAR             -> Var(translate (child tok 0))
          | ES3Parser.ASSIGN          -> Assign(translate (child tok 0), translate (child tok 1))
          | ES3Parser.Identifier      -> Identifier(text tok)
          | ES3Parser.StringLiteral   -> String(jsString tok)
          | ES3Parser.DecimalLiteral  -> Number(double (text tok))
          | ES3Parser.WITH            -> With(translate (child tok 0), translate(child tok 1))

          | ES3Parser.FUNCTION -> 
            Function(Scope.New [for x in (children (child tok 0)) -> text x], translate (child tok 1))

          | _ -> failwithf "No parser for token %s (%i)" (ES3Parser.tokenNames.[tok.Type]) tok.Type
  
        (*
        Parser
        *)
        let parse str = 
          let lexer = new Xebic.ES3.ES3Lexer(new Antlr.Runtime.ANTLRStringStream(str))
          let parser = new Xebic.ES3.ES3Parser(new Antlr.Runtime.CommonTokenStream(lexer))
          Function(Scope.New [], translate (parser.program().Tree :?> AntlrToken))


