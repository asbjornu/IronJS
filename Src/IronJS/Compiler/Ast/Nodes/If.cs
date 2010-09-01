using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class If : Node {
        public INode Test { get { return Children[0]; } }
        public INode IfTrue { get { return Children[1]; } }
        public INode IfFalse { get { return Children[1]; } }

        If(SourcePosition pos, INode[] children)
            : base(pos, children) {
        }

        public override INode Clone(INode[] children) {
            return new If(Source, children);
        }

        public static INode Create(SourcePosition position, INode test, INode ifTrue, INode ifFalse) {
            return new If(position, new[] { test, ifTrue, ifFalse });
        }
    }
}
