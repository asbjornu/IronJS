using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Invoke : Node {
        public INode Target { get { return Children[0]; } }
        public INode[] Arguments { get { return Children[0].Children; } }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        public Invoke(SourcePosition pos, INode target, INode[] arguments)
            : base(pos) {
                Children = new[] { target, new Node(arguments) };
        }

        public override INode Clone() {
            return new Invoke(Source, Target, Arguments);
        }
    }
}
