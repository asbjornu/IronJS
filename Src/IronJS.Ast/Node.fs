namespace IronJS.Ast

type BinaryOp 
  //Math
  = Add
  | Sub

type UnaryOp 
  = Inc
  | Dec

type Node 
  //Constants
  = String of string
  | Number of double
  | Boolean of bool
  | Pass
  | Null
  | Undefined

  //Ops
  | Binary of BinaryOp * Node * Node
  | Unary of UnaryOp * Node

  //
  | Block of Node list
  | Identifier of string
  | Var of Node
  | Return of Node
  | With of Node

