namespace IronJS

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

    [<System.Flags>]
    type VariableOptions 
      = Nothing = 0
      | IsParameter = 1
      | NeedsProxy = 2
      | IsClosedOver = 4
      | InitToUndefined = 8
        
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
      | Function    of string option * Scope * Tree
      | Typed       of Types.JsType * Tree
  
    (*
    *)
    and Variable = {
      Name: string
      Index: int
      Type: Types.JsType
      Options: VariableOptions
      AssignedFrom: Set<Tree>
    } with 
      static member New = {
        Name = ""
        Index = -1
        Type = Types.JsType.Nothing
        Options = VariableOptions.Nothing
        AssignedFrom = Set.empty
      }

      member x.AddAssignedFrom tree =
        {x with AssignedFrom = x.AssignedFrom.Add tree}

      member x.AddOption option = 
        {x with Options = x.Options ||| option}

      member x.HasOption option = 
        x.Options.HasFlag(option)

      member x.IsParameter =
        x.Options.HasFlag(VariableOptions.IsParameter)

      member x.IsClosedOver =
        x.Options.HasFlag(VariableOptions.IsClosedOver)

      member x.NeedsProxy =
        x.Options.HasFlag(VariableOptions.NeedsProxy)

      member x.InitToUndefined =
        x.Options.HasFlag(VariableOptions.InitToUndefined)
      
    (*
    *)
    and Closure = {
      Name: string
      Index: int
      FromScope: int
      DynamicScopeLevel: int
    } with
      static member New = {
        Name = ""
        Index = -1
        FromScope = -1
        DynamicScopeLevel = -1
      }
      
    (*
    *)
    and [<CustomEquality; CustomComparison>] Scope = {
      Variables: Set<Variable>
      Closures: Set<Closure>
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
        
      member x.AddVariable func = 
        {x with Variables = x.Variables.Add(func Variable.New)}
        
      member x.AddClosure func = 
        {x with Closures = x.Closures.Add(func {Closure.New with Index = x.Closures.Count})}
        
      member x.TryGetVariable name = 
        Seq.tryFind (fun (v:Variable) -> v.Name = name) x.Variables
        
      member x.TryGetClosure name =
        Seq.tryFind (fun c -> c.Name = name) x.Closures
        
      static member New parms = {
        Variables = parms
                    |>  Seq.mapi (fun i x -> 
                          {Variable.New with 
                            Name = x; 
                            Index = i; 
                            Options = VariableOptions.IsParameter
                          }
                        )  
                    |>  Set.ofSeq

        Closures = Set.empty
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
      | Function(name, scope, tree) -> Function(name, scope, func tree) 

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
              | Function(_, _, _) -> Typed(Types.JsType.Function, Pass)
              | _ -> rtree

            scopes := scope.UpdateVariable name (fun (v:Variable) -> v.AddAssignedFrom tree) :: List.tail !scopes

          //Return node
          Assign(Identifier(name), analyze rtree)
          
        | Function(name, scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(name, scope, tree)

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
            
        | Function(name, scope, tree) ->
          let scope, tree = doInsideScope scopes {scope with DynamicScopeLevel = !dynamicScopeLevel} (fun () -> analyze tree)
          Function(name, scope, tree)

        | tree -> walk analyze tree

      analyze tree
      
    (*
    *)
    let stripVarStatements tree =
      let scopes = ref List.empty

      let updateTopScope name =
        if (!scopes).Length > 0 then 
          let scope:Scope = List.head !scopes
          scopes := scope.AddVariable (fun v -> {v with Name = name}) :: List.tail !scopes

      let rec strip tree =
        match tree with
        | Var(Assign(Identifier(name), rtree)) ->
          updateTopScope name
          Assign(Identifier(name), strip rtree)

        | Var(Identifier(name)) ->
          updateTopScope name
          Pass

        | Function(name, scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> strip tree)
          Function(name, scope, tree)

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
                    | Some(v:Variable)  -> 
                      let scope = 
                        if v.Options.HasFlag(VariableOptions.IsClosedOver)
                          then itm
                          else itm.UpdateVariable name (fun v -> v.AddOption VariableOptions.IsClosedOver)
                          
                      Some(scopes.Length, itm.DynamicScopeLevel), scope :: scopes

                  | Some(fromScope, dynamicScopeLevel)  -> 

                    let scope = 
                      match itm.TryGetClosure name with
                      | Some(_) ->  itm
                      | None ->  itm.AddClosure (
                                      fun c -> {c with 
                                                  Name = name; 
                                                  FromScope = fromScope; 
                                                  DynamicScopeLevel = dynamicScopeLevel})
                    level, scope :: scopes

                ) !scopes (None, [])

              //Store updated scopes
              scopes := newScopes

              //Return tree
              tree

            | Some(_) ->  tree
          | Some(_)   ->  tree

        | Function(name, scope, tree) ->
          let scope, tree = doInsideScope scopes scope (fun () -> analyze tree)
          Function(name, scope, tree)

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
            Function(None, Scope.New [for x in (children (child tok 0)) -> text x], translate (child tok 1))

          | _ -> failwithf "No parser for token %s (%i)" (ES3Parser.tokenNames.[tok.Type]) tok.Type
  
        (*
        Parser
        *)
        let parse str = 
          let lexer = new Xebic.ES3.ES3Lexer(new Antlr.Runtime.ANTLRStringStream(str))
          let parser = new Xebic.ES3.ES3Parser(new Antlr.Runtime.CommonTokenStream(lexer))
          Function(None, Scope.New [], translate (parser.program().Tree :?> AntlrToken))


