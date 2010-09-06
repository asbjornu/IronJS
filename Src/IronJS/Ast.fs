namespace IronJS

  module Ast = 

    open IronJS
    open IronJS.Utils

    module Var = 

      type Opts 
        = IsParameter
        | NeedsProxy
        | IsClosedOver
        | InitToUndefined
        | ForceDynamic

      let name          = Quad.fst
      let index         = Quad.snd
      let options       = Quad.trd
      let assignedFrom  = Quad.fth

      let isClosedOver (_, _, o:Set<_>, _)    = o.Contains Opts.IsClosedOver
      let needsProxy (_, _, o:Set<_>, _)      = o.Contains Opts.NeedsProxy
      let isParameter (_, _, o:Set<_>, _)     = o.Contains Opts.IsParameter
      let forceDynamic (_, _, o:Set<_>, _)     = o.Contains Opts.ForceDynamic
      let initToUndefined (_, _, o:Set<_>, _) = o.Contains Opts.InitToUndefined

      let addOpt (n, i, o:Set<_>, a) opt      = n, i, o.Add opt, a
      let addOpt2 opt (n, i, o:Set<_>, a)     = n, i, o.Add opt, a
      let delOpt (n, i, o:Set<_>, a) opt      = n, i, o.Remove opt, a
      let delOpt2 opt (n, i, o:Set<_>, a)     = n, i, o.Remove opt, a
      let addAssign (n, i, o, a:Set<_>) tree  = n, i, o, a.Add tree

      let newParam i n = n, i, set [Opts.IsParameter], Set.empty

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
        
    (*
    *)
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
      | Eval        of Tree
      | Identifier  of string
      | Var         of Tree
      | Return      of Tree
      | With        of Tree * Tree
      | Function    of Scope * Tree
      | Typed       of Types.JsType * Tree
      | Invoke      of Tree * Tree list
      | New         of Types.JsType * Option<Tree> * Option<Tree list>
      | Property    of Tree * string

    and ScopeOpts
      = HasEval
      | HasWith
      
    and Scope = {
      Variables: Set<string * int * Set<Var.Opts> * Set<Tree>>
      Closures: Set<string * int * int * int>
      Options: Set<ScopeOpts>
      WithLevel: int
    } with

      member x.UpdateVariable name func = 
        match x.TryGetVariable name with
        | None -> failwith "Que?"
        | Some(v) -> {x with Variables = x.Variables.Remove(v).Add(func v)}
        
      member x.AddVariable var = {x with Variables = x.Variables.Add(var)}
      member x.AddClosure closure = {x with Closures = x.Closures.Add(closure)}

      member x.TryGetVariable name = Seq.tryFind (fun (n, _, _, _) -> n = name) x.Variables
      member x.TryGetClosure name = Seq.tryFind (fun (n, _, _, _) -> n = name) x.Closures

      member x.AddOpt opt = {x with Options = x.Options.Add opt}
      member x.VarCount = x.Variables.Count
        
      static member New parms = {
        Closures = set[]
        Variables = ["~closure"] @ parms |> Seq.mapi Var.newParam |> Set.ofSeq
        WithLevel = 0
        Options = set[]
      }
        
    (*
    *)
    let private doInsideScope scopes scope func =
      scopes     := scope :: !scopes
      let result  = func()
      let scope   = List.head !scopes
      scopes     := List.tail !scopes
      scope, result
        
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
      | New(type', ftree, itrees) ->
        
        let ftree =
          match ftree with
          | None -> None
          | Some(tree) -> Some(func tree)

        let itrees =
          match itrees with
          | None -> None
          | Some(trees) -> Some([for tree in trees -> func tree])

        New(type', ftree, itrees)
        
      | Eval(tree)           -> Eval(func tree)
      | Property(tree, name) -> Property(func tree, name)
      | Assign(ltree, rtree) -> Assign(func ltree, (func rtree))
      | Block(trees) -> Block([for tree in trees -> func tree])
      | Var(tree) -> Var(func tree)
      | Return(tree) -> Return(func tree)
      | With(target, tree) -> With(func target, func tree)
      | Function( scope, tree) -> Function(scope, func tree) 
      | Invoke(tree, trees) -> Invoke(func tree, [for tree in trees -> func tree])

    (*
    *)
    let analyzeAssignment tree =
      let scopes = ref List.empty<Scope>

      let rec analyze tree =
        match tree with
        | Assign(Identifier(name), rtree) ->
          let scope = List.head !scopes

          //Update scopes
          match scope.TryGetVariable name with
          | None -> ()
          | _ ->  

            let tree = 
              match rtree with
              | Function(_, _) -> Typed(Types.JsType.Function, Pass)
              | _ -> rtree

            scopes := scope.UpdateVariable name (fun v -> Var.addAssign v tree) :: List.tail !scopes

          //Return node
          Assign(Identifier(name), analyze rtree)
          
        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(scope, tree)

        | _ -> walk analyze tree

      analyze tree
      
    (*
    *)
    let stripVarDeclarations tree =
      let scopes = ref List.empty<Scope>

      let updateTopScope name opts =
        if (!scopes).Length > 1 then 
          let scope = List.head !scopes
          scopes := scope.AddVariable (name, scope.VarCount, opts, set[]) :: List.tail !scopes

      let rec strip tree =
        match tree with
        | Var(Assign(Identifier(name), rtree)) ->
          updateTopScope name (set[])
          Assign(Identifier(name), strip rtree)

        | Var(Identifier(name)) ->
          updateTopScope name (set[Var.InitToUndefined])
          Pass

        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> strip tree)
          Function(scope, tree)

        | tree -> walk strip tree

      strip tree

    let analyzeEval tree =
      let scopes = ref List.empty<Scope>

      let forceClosure var =
        match var with
        | "~closure", _, _, _ -> var
        | _ -> var |> Var.addOpt2 Var.ForceDynamic

      let rec analyze tree =
        match tree with
        | Invoke(Identifier("eval"), expr :: []) ->
          let newScopes = [for scope in !scopes -> {scope with Variables = Set.map forceClosure scope.Variables}]
          let topScope  = (List.head newScopes).AddOpt (ScopeOpts.HasEval)

          scopes := topScope :: List.tail newScopes
          Eval(expr)

        | Function(scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(scope, tree)

        | tree -> walk analyze tree

      analyze tree

    (*
    *)
    let analyzeClosureScopes tree =
      let scopes = ref List.empty<Scope>

      let rec analyze tree =
        match tree with
        | Identifier(name) -> 

          let scope = List.head !scopes

          //Check if we have a local varible by name
          match scope.TryGetVariable name with
          | None  -> 
            //Check if we already have a closure variable for this name
            match scope.TryGetClosure name with
            | None  -> 
              //Build new scope objects with closure variables set
              let _, newScopes = 
                List.foldBack (fun (itm:Scope) (level, scopes) ->
                  match level with
                  //We havn't found the scope with the variable in yet
                  | None  ->

                    match itm.TryGetVariable name with
                    //Not in this scope either
                    | None     ->  None, itm :: scopes

                    //Found in this scope
                    | Some(v)  -> 

                      //Update scope and add the Var.Opts.IsClosedOver
                      //value to the local varible in this scope
                      let scope = 
                        if Var.isClosedOver v
                          then itm
                          else itm.UpdateVariable name (fun v -> Var.addOpt2 Var.Opts.IsClosedOver v)
                          
                      Some(scopes.Length, Quad.snd v), scope :: scopes

                  //We have found the scope
                  | Some(fromScope, indexInScope)  -> 

                    //Make sure the current scope closes over the variable
                    let scope = 
                      match itm.TryGetClosure name with
                      | Some(_) ->  itm
                      | None    ->  itm.AddClosure (name, -1, fromScope, indexInScope)

                    level, scope :: scopes

                ) !scopes (None, [])

              //Store updated scopes
              scopes := newScopes

              //Return tree
              tree

            //Already have variable as closure
            | Some(_) ->  tree

          //Local variable exists
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
          | ES3Parser.WITH            -> With(translate (child tok 0), translate (child tok 1))
          | ES3Parser.CALL            -> Invoke(translate (child tok 0), [for x in (children (child tok 1)) -> translate x])
          | ES3Parser.OBJECT          -> New(Types.JsType.Object, None, None)
          | ES3Parser.BYFIELD         -> Property(translate (child tok 0), text (child tok 1))
          | ES3Parser.RETURN          -> Return(translate (child tok 0))

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


