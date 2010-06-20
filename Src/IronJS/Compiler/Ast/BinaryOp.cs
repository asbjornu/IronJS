using System;

namespace IronJS.Compiler.Ast {
    public enum BinaryOp {
        Assign  = 1, 
        Add     = 2, 
        Sub     = 3, 
        Div     = 4, 
        Mul     = 5, 
        Mod     = 6, 
        Eq      = 100, 
        EqEq    = 101, 
        Lt      = 102, 
        Gt      = 103, 
        GtEq    = 104, 
        LtEq    = 105, 
        NotEq   = 106
    }
}
