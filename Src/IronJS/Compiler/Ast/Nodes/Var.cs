using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Var : Node {
        public INode Node { get { return Children[0]; } }

        Var(SourcePosition position, INode[] children)
            : base(position, children) {
        }

        public static INode Create(SourcePosition pos, INode node) {
            return new Var(pos, new[] { node });
        }
    }
}
