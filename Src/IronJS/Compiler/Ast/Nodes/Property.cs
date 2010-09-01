using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Property : Node {
        public enum AccessMode {
            Field, Index
        }

        public INode Target { get { return Children[0]; } }
        public INode Member { get { return Children[1]; } }
        public AccessMode Mode { get; protected set; }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        Property(SourcePosition pos, INode[] children, AccessMode mode)
            : base(pos, children) {
        }

        public override INode Clone(INode[] children) {
            return new Property(Source, children, Mode);
        }

        public static INode Field(SourcePosition pos, INode target, INode member) {
            return new Property(pos, new[] { target, member }, AccessMode.Field);
        }

        public static INode Index(SourcePosition pos, INode target, INode member) {
            return new Property(pos, new[] { target, member }, AccessMode.Index);
        }
    }
}
