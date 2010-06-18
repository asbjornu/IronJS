using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class For : Node {
        public INode Init { get { return Children[0]; } }
        public INode Test { get { return Children[1]; } }
        public INode Incr { get { return Children[2]; } }
        public INode Body { get { return Children[3]; } }

        public For(SourcePosition pos, INode init, INode test, INode incr, INode body)
            : base(pos) {
                Children = new[] { init, test, incr, body };
        }
    }
}
