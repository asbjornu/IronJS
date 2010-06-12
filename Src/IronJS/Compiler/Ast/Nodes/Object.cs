using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class NewObject : Node {
        public Tuple<INode, INode>[] InitExpressions { get; private set; }

        public NewObject(SourcePosition pos)
            : this(pos, new Tuple<INode, INode>[0]) {
        }

        public NewObject(SourcePosition pos, Tuple<INode, INode>[] initExpressions)
            : base(pos) {
                InitExpressions = initExpressions;
        }
    }
}
