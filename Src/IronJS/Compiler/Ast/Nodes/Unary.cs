using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Unary : Node {
        public enum OpType {
            Not, Delete, Void, PostInc,
            PostDec, Inc, Dec
        }

        public INode Target { get { return Children[0]; } }
        public OpType Op { get; private set; }

        public Unary(SourcePosition pos, INode target, OpType op)
            : base(pos) {
                Children = new[] { target };
                Op = op;
        }
    }
}
