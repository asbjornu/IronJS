using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class If : Node {
        public INode Test { get { return Children[0]; } }
        public INode IfTrue { get { return Children[1]; } }
        public INode IfFalse { get { return Children[1]; } }

        public If(SourcePosition position, INode test, INode ifTrue, INode ifFalse)
            : base(position) {
                Children = new[] { test, ifTrue, ifFalse };
        }

        public override INode Clone() {
            return new If(Source, Test, IfTrue, IfFalse);
        }
    }
}
