using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class With : Node {
        public INode Target { get { return Children[0]; } }
        public INode Block { get { return Children[1]; } }

        With(SourcePosition pos, INode[] children)
            : base(pos, children) {
        }

        public static INode Create(SourcePosition pos, INode obj, INode body) {
            return new With(pos, new[] { obj, body });
        }
    }
}
