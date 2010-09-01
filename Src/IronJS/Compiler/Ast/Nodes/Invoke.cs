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

        Invoke(SourcePosition pos, INode[] children)
            : base(pos, children) {
        }

        public override INode Clone(INode[] children) {
            return new Invoke(Source, children);
        }

        public static INode Create(SourcePosition pos, INode target, INode[] arguments) {
            return new Invoke(pos, new[] { target, Node.Create(arguments) });
        }
    }
}
