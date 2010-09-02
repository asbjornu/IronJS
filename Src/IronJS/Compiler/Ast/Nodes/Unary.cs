using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Unary : Node {
        public enum OpType {
            Not, Delete, Void, PostInc,
            PostDec, Inc, Dec,
            Return
        }

        public INode Target {
            get {
                return Children[0];
            }
        }

        public OpType Op {
            get;
            protected set;
        }

        Unary(SourcePosition pos, INode[] children, OpType op)
            : base(pos, children) {
                Op = op;
        }

        public static INode Create(SourcePosition pos, INode target, OpType op) {
            return new Unary(pos, new[] { target }, op);
        }
    }
}
