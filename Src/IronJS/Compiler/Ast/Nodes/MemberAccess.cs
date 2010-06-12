
namespace IronJS.Compiler.Ast.Nodes {
    public class MemberAccess : Node {
        public INode Target { get; private set; }
        public INode Member { get; private set; }
        public MemberAccessType Type { get; private set; }

        public MemberAccess(SourcePosition pos, INode target, INode member, MemberAccessType type)
            : base(pos) {
                Target = target;
                Member = member;
                Type = type;
        }
    }
}
