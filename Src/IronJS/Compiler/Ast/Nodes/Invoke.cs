using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Invoke : Node {
        public INode Target { get { return Children[0]; } }
        public INode[] Arguments { get { return Children[0].Children; } }

        public override bool TypeIsStatic {
            get {
                return true;
            }
        }

        public Invoke(SourcePosition pos, INode target, INode[] arguments)
            : base(pos) {
                Children = new[] { target, new Node(arguments) };
        }
    }
}
