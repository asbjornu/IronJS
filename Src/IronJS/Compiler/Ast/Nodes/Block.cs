namespace IronJS.Compiler.Ast.Nodes {
    public class Block : Node {
        public INode[] Nodes { get; private set; }

        public Block(SourcePosition pos, INode[] nodes)
            : base(pos) {
                Nodes = nodes;
        }
    }
}
