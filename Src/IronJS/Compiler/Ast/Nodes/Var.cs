using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Var : Node {
        public INode Node { get { return Children[0]; } }

        public Var(SourcePosition position, INode node)
            : base(position) {
                Children = new[] { node };
        }
    }
}
