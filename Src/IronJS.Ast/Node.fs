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
  = String    of IronJS.Type * string
  | Number    of IronJS.Type * double
  | Boolean   of IronJS.Type * bool
  | Pass      of IronJS.Type
  | Null      of IronJS.Type
  | Undefined of IronJS.Type

  //Ops
  | Binary  of IronJS.Type * BinaryOp * Node * Node
  | Unary   of IronJS.Type * UnaryOp  * Node

  //
  | Assign      of IronJS.Type * Node * Node
  | Block       of IronJS.Type * Node list
  | Identifier  of IronJS.Type * string
  | Var         of IronJS.Type * Node
  | Return      of IronJS.Type * Node
  | With        of IronJS.Type * Node