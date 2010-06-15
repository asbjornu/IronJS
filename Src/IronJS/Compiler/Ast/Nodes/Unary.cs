using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Unary : Node {
        public INode Target { get; private set; }
        public UnaryOp Op { get; private set; }

        public Unary(SourcePosition pos, INode target, UnaryOp op)
            : base(pos) {
                Target = target;
                Op = op;
        }
    }
}
