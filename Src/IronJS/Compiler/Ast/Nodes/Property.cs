using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Property : Node {
        public enum AccessMode {
            Field, Index
        }

        public INode Target { get; private set; }
        public INode Member { get; private set; }
        public AccessMode Mode { get; private set; }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        public Property(SourcePosition pos, INode target, INode member, AccessMode mode)
            : base(pos) {
                Target = target;
                Member = member;
                Mode = mode;
        }
    }
}
