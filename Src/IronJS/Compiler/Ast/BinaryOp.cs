using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast {
    public enum BinaryOp {
        Assign, Add, Sub, Div, Mul, Mod, 
        Eq, EqEq, Lt, Gt, GtEq, LtEq, NotEq
    }
}
