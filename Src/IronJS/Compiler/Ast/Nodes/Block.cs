using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Block : Node {
        public INode[] Nodes { get { return Children; } }

        Block(SourcePosition pos, INode[] nodes)
            : base(pos, nodes) {
        }

        public override INode Clone(INode[] children) {
            return new Block(Source, children);
        }

        public static new INode Create(SourcePosition pos, INode[] nodes) {
            return new Block(pos, nodes);
        }
    }
}
