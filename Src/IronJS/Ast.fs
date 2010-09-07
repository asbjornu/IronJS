namespace IronJS

  module Ast = 

    open IronJS
    open IronJS.Utils

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
      | Function    of MetaData * Tree
      | Typed       of Types.JsType * Tree
      | Invoke      of Tree * Tree list
      | New         of Types.JsType * Option<Tree> * Option<Tree list>
      | Property    of Tree * string

    and VariableFlags 
      = Nothing = 0
      | Parameter = 1
      | InitToUndefined = 2
      | ForceDynamic = 4

    and Variable = {
      Name: string
      Index: int
      Flags: VariableFlags
      AssignedFrom: Set<Tree>
      StaticType: Option<Types.JsType>
    } with
      member x.IsParameter = x.Flags.HasFlag(VariableFlags.Parameter)
      member x.IsForcedDynamic = x.Flags.HasFlag(VariableFlags.ForceDynamic)
      member x.InitToUndefined = x.Flags.HasFlag(VariableFlags.InitToUndefined)
      member x.SetFlag flag = {x with Flags = x.Flags ||| flag}
      member x.RemoveFlag flag = {x with Flags = (x.Flags ||| flag) ^^^ flag}
      member x.AddAssignedFrom tree = {x with AssignedFrom = x.AssignedFrom.Add tree}
      member x.HasStaticType = x.StaticType <> None

      static member New name index flags = {
        Name = name
        Index = index
        Flags = flags
        AssignedFrom = Set.empty
        StaticType = None
      }

      static member NewParam name index = 
        Variable.New name index VariableFlags.Parameter

    and Closure = {
      Name: string
      Indexes: int * int
    }

    and MetaDataFlags 
      = Nothing = 0
      | RecursiveLookup = 1
      | RequiresParentScope = 2

    and MetaData = {
      Variables: Set<Variable>
      Closures: Set<Closure>
      Flags: MetaDataFlags
      VariableIndexMap: Map<string, int>
    } with
      member x.SetFlag flag = {x with Flags = x.Flags ||| flag}
      member x.RemoveFlag flag = {x with Flags = (x.Flags ||| flag) ^^^ flag}
      member x.RequiresRecursiveVarLookup = x.Flags.HasFlag(MetaDataFlags.RecursiveLookup)

      member x.AddVariable var = {x with Variables = x.Variables.Add var}
      member x.AddClosure cls = {x with Closures = x.Closures.Add cls}
      member x.VariableCount = x.Variables.Count
      
      member x.TryGetVariable name = 
        Seq.tryFind (fun (var:Variable) -> var.Name = name) x.Variables
        
      member x.TryGetClosure name = 
        Seq.tryFind (fun cls -> cls.Name = name) x.Closures

      member x.UpdateVariable func name =
        match x.TryGetVariable name with
        | None -> failwith "Que?"
        | Some(var) -> 
          {x with
            Variables = Set.add (func var) (Set.remove var x.Variables)
          }

      static member New paramNames = {
        Variables =
          ["~closure"] @ paramNames
            |> Seq.mapi (fun index name -> Variable.NewParam name index)
            |> Set.ofSeq

        Closures = Set.empty
        Flags = MetaDataFlags.Nothing
        VariableIndexMap = Map.empty
      }
        
    (*
    *)
    let private _walk func tree = 
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
        
      | Eval(tree)              -> Eval(func tree)
      | Property(tree, name)    -> Property(func tree, name)
      | Assign(ltree, rtree)    -> Assign(func ltree, (func rtree))
      | Block(trees)            -> Block([for tree in trees -> func tree])
      | Var(tree)               -> Var(func tree)
      | Return(tree)            -> Return(func tree)
      | With(target, tree)      -> With(func target, func tree)
      | Function( scope, tree)  -> Function(scope, func tree) 
      | Invoke(tree, trees)     -> Invoke(func tree, [for tree in trees -> func tree])
        
    let private _pushMetaData metaData chain =
      chain := metaData :: !chain

    let private _popMetaData chain =
      match !chain with
      | [] -> failwith "Que?"
      | metaData::chain' ->
        chain := chain'
        metaData

    //-------------------------------------------------------------------------
    // Resolves the type of an Ast.Tree object, invoking 'func'
    // to resolve the type of Ast.Identifier objects
    let rec resolveAstType tree func =
      match tree with
      | Identifier(name)          -> func name
      | Boolean(_)                -> Types.JsType.Boolean
      | String(_)                 -> Types.JsType.String
      | Number(_)                 -> Types.JsType.Number
      | Typed(type', _)           -> type'
      | Null                      -> Types.JsType.Null
      | Undefined                 -> Types.JsType.Undefined
      | Function(_, _)            -> Types.JsType.Function
      | Unary(op, tree)           -> resolveAstType tree func
      | Binary(op, ltree, rtree)  -> resolveAstType ltree func ||| resolveAstType rtree func
      | New(type', _, _)          -> type'
      | Property(_, _)            -> Types.JsType.Dynamic
      | _                         -> Types.JsType.Nothing

    let analyzeStaticTypes tree =
      let metaDataChain = ref List.empty<MetaData>

      let rec analyze tree =
        match tree with
        | Function(metaData, tree) ->
          let tree' = analyze tree

          let typeResolver state tree =
            state ||| resolveAstType tree (fun _ -> raise (new System.Exception()))

          let variables' =
            metaData.Variables
              |>  Set.map (fun var ->
                    try
                      let staticType = 
                        var.AssignedFrom
                          |>  Seq.fold typeResolver Types.JsType.Nothing

                      match staticType with
                      | Types.JsType.Nothing -> var
                      | _ ->  {var with 
                                StaticType = Some(staticType); 
                                AssignedFrom = Set.empty
                              }
                    with _ -> var
                  )

          let metaData' = {metaData with Variables = variables'}
          Function(metaData', tree')

        | _ -> _walk analyze tree

      analyze tree

    let analyzeAssignment tree =
      let metaDataChain = ref List.empty<MetaData>

      let rec analyze tree =
        match tree with
        | Assign(Identifier(name), rtree) ->
          let metaData = List.head !metaDataChain

          //Update scopes
          match metaData.TryGetVariable name with
          | None -> ()
          | _ ->  

            let tree = 
              match rtree with
              | Function(_, _) -> Typed(Types.JsType.Function, Pass)
              | _ -> rtree

            let metaData' = metaData.UpdateVariable (fun var -> var.AddAssignedFrom tree) name
            metaDataChain := metaData' :: List.tail !metaDataChain

          //Return node
          Assign(Identifier(name), analyze rtree)
          
        | Function(metaData, tree) ->
          _pushMetaData metaData metaDataChain
          let tree' = analyze tree
          let metaData' = _popMetaData metaDataChain
          Function(metaData', tree')

        | _ -> _walk analyze tree

      analyze tree
      
    let stripVarDeclarations tree =
      let metaDataChain = ref List.empty<MetaData>

      let addVariableToMetaData name flags =
        if (!metaDataChain).Length > 1 then 
          let metaData = List.head !metaDataChain
          let metaData' = metaData.AddVariable (Variable.New name metaData.VariableCount flags)
          metaDataChain := metaData' :: List.tail !metaDataChain

      let rec strip tree =
        match tree with
        | Var(Assign(Identifier(name), rtree)) ->
          addVariableToMetaData name (VariableFlags.Nothing)
          Assign(Identifier(name), strip rtree)

        | Var(Identifier(name)) ->
          addVariableToMetaData name (VariableFlags.InitToUndefined)
          Pass

        | Function(metaData, tree) ->
          _pushMetaData metaData metaDataChain
          let tree' = strip tree
          let metaData'1 = _popMetaData metaDataChain
          let metaData'2 =
            {metaData'1 with
              VariableIndexMap = 
                metaData'1.Variables
                  |> Seq.map (fun var -> var.Name, var.Index)
                  |> Map.ofSeq
            }
          Function(metaData'2, tree')

        | tree -> _walk strip tree

      strip tree
      
    let detectEval tree =
      let metaDataChain = ref List.empty<MetaData>

      let forceDynamicVariables metaData =
        let forceDynamic (var:Variable) =
          var.SetFlag VariableFlags.ForceDynamic

        let variables' =
          metaData.Variables
            |> Set.filter (fun (var:Variable) -> var.Name.[0] <> '~')
            |> Set.map forceDynamic

        {metaData with Variables = variables'}

      let rec detectEval' tree =
        match tree with
        | Invoke(Identifier("eval"), expr :: []) ->
          let metaDataChain'  = [for metaData in !metaDataChain -> forceDynamicVariables metaData]
          let topMetaData     = (List.head metaDataChain').SetFlag MetaDataFlags.RecursiveLookup

          metaDataChain := topMetaData :: (List.tail metaDataChain')

          Eval(expr)

        | Function(metaData, tree) ->
          _pushMetaData metaData metaDataChain
          let tree' = detectEval' tree
          let metaData' = _popMetaData metaDataChain
          Function(metaData', tree')

        | tree -> _walk detectEval' tree

      detectEval' tree
      
    let analyzeClosureScopes tree =
      let metaDataChain = ref List.empty<MetaData>

      let rec analyze tree =
        match tree with
        | Identifier(name) -> 

          let metaData = List.head !metaDataChain

          match metaData.TryGetVariable name  with
          | None  -> 
            match metaData.TryGetClosure name with
            | None  -> 

              //Build new scope objects with closure variables set
              let state, metaDataChain' = 
                List.foldBack (fun (metaData:MetaData) (state, metaDataAcc:MetaData list) ->
                  match state with
                  //We havn't found the scope with the variable in yet
                  | None ->
                    let state = 
                      match metaData.TryGetVariable name with
                      | None     -> None //Not in this scope either
                      | Some(v)  -> Some(metaDataAcc.Length, v.Index) //Found in this scope
                    state, metaData :: metaDataAcc

                  | Some(fromScope:int, indexInScope:int) -> 
                    let metaData' = metaData.SetFlag MetaDataFlags.RequiresParentScope
                    Some(fromScope, indexInScope), metaData' :: metaDataAcc

                ) !metaDataChain (None, [])

              match state with
              | None -> ()
              | Some(fromScope, indexInScope) ->
                  let metaData' = (List.head metaDataChain').AddClosure {Name = name; Indexes = (fromScope, indexInScope)}
                  metaDataChain :=  metaData' :: (List.tail metaDataChain')

              //Return tree
              tree

            //Already have variable as closure
            | Some(_) ->  tree

          //Local variable exists
          | Some(_)   ->  tree
          
        | Function(metaData, tree) ->
          _pushMetaData metaData metaDataChain
          let tree' = analyze tree
          let metaData' = _popMetaData metaDataChain
          Function(metaData', tree')

        | tree -> _walk analyze tree

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
            let parameterNames = [for x in (children (child tok 0)) -> text x]
            Function(MetaData.New parameterNames , translate (child tok 1))

          | _ -> failwithf "No parser for token %s (%i)" (ES3Parser.tokenNames.[tok.Type]) tok.Type
  
        (*
        Parser
        *)
        let parse str = 
          let lexer = new Xebic.ES3.ES3Lexer(new Antlr.Runtime.ANTLRStringStream(str))
          let parser = new Xebic.ES3.ES3Parser(new Antlr.Runtime.CommonTokenStream(lexer))
          Function(MetaData.New [], translate (parser.program().Tree :?> AntlrToken))


