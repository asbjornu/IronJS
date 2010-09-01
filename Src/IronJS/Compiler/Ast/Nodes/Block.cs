using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Block : Node {
        public INode[] Nodes { get { return Children; } }

        public Block(SourcePosition pos, INode[] nodes)
            : base(pos) {
                Children = nodes; 
        }
    }
}
