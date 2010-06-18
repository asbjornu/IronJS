using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Property : Node {
        public enum AccessMode {
            ByField, ByIndex
        }

        public INode Target { get { return Children[0]; } }
        public INode Member { get { return Children[1]; } }
        public AccessMode Mode { get; private set; }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        public Property(SourcePosition pos, INode target, INode member, AccessMode mode)
            : base(pos) {
                Children = new[] { target, member };
                Mode = mode;
        }
    }
}
