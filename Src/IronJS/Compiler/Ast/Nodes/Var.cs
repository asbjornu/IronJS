using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Var : Node {
        public INode Node { get { return Children[0]; } }

        public Var(SourcePosition position, INode[] children)
            : base(position, children) {
        }

        public Var(SourcePosition pos, INode var)
            : this(pos, new[] { var }) {
        }
    }
}
