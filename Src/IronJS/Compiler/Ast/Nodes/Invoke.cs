using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Invoke : Node {
        public INode Target { get; private set; }
        public INode[] Arguments { get; private set; }

        public override bool StaticType {
            get {
                return true;
            }
        }

        public Invoke(SourcePosition pos, INode target, INode[] arguments)
            : base(pos) {
                Target = target;
                Arguments = arguments;
        }
    }
}
