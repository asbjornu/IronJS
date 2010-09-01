using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class For : Node {
        public INode Init { get { return Children[0]; } }
        public INode Test { get { return Children[1]; } }
        public INode Incr { get { return Children[2]; } }
        public INode Body { get { return Children[3]; } }

        For(SourcePosition pos, INode[] children)
            : base(pos, children) {
        }

        public override INode Clone(INode[] children) {
            return new For(Source, children);
        }

        public static INode Create(SourcePosition pos, INode init, INode test, INode incr, INode body) {
            return new For(pos, new[] { init, test, incr, body });
        }
    }
}
