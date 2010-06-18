using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Unary : Node {
        public INode Target { get { return Children[0]; } }
        public UnaryOp Op { get; private set; }

        public Unary(SourcePosition pos, INode target, UnaryOp op)
            : base(pos) {
                Children = new[] { target };
                Op = op;
        }
    }
}
