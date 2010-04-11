﻿module IronJS.Utils

open System

(*Public Type Aliases*)
type MetaObj        = System.Dynamic.DynamicMetaObject

type Et             = System.Linq.Expressions.Expression
type EtParam        = System.Linq.Expressions.ParameterExpression
type EtLambda       = System.Linq.Expressions.LambdaExpression

type AstUtils       = Microsoft.Scripting.Ast.Utils
type DynamicUtils   = Microsoft.Scripting.Utils.DynamicUtils

type CtorInfo       = System.Reflection.ConstructorInfo
type ParmInfo       = System.Reflection.ParameterInfo
type FieldInfo      = System.Reflection.FieldInfo
type MethodInfo     = System.Reflection.MethodInfo

type AstTree        = Antlr.Runtime.Tree.CommonTree

type ClrType        = System.Type
type Dynamic        = System.Object
type StrongBox<'a>  = System.Runtime.CompilerServices.StrongBox<'a>
type JitCache       = System.Collections.Concurrent.ConcurrentDictionary<System.Type, System.Delegate>


let toList<'a> (ilst:System.Collections.IList) =
  match ilst with
  | null -> []
  | _    -> let rec convert (lst:System.Collections.IList) n =
              if n = lst.Count 
                then []
                else (lst.[n] :?> 'a) :: convert lst (n+1)

            convert ilst 0

let getCtor (typ:Type) (args:Type list) =
  let rec matchArgs args (parms:ParmInfo list) = 
    match args with
    | []      -> true
    | xA::xsA -> match parms with
                 | []      -> failwith "Should never happen"
                 | xP::xsP -> if xP.ParameterType.IsAssignableFrom(xA)
                                then matchArgs xsA xsP
                                else false

  Array.find (fun (ctor:CtorInfo) ->
    let parms = List.ofArray (ctor.GetParameters())
    if args.Length = parms.Length 
      then matchArgs args parms 
      else false
  ) (typ.GetConstructors())