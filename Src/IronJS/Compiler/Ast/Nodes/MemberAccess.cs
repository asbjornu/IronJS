
namespace IronJS.Compiler.Ast.Nodes {
    public class MemberAccess : Node {
        public enum AccessType {
            ByField, ByIndex
        }

        public INode Target { get; private set; }
        public INode Member { get; private set; }
        public AccessType Type { get; private set; }

        public MemberAccess(SourcePosition pos, INode target, INode member, AccessType type)
            : base(pos) {
                Target = target;
                Member = member;
                Type = type;
        }
    }
}
