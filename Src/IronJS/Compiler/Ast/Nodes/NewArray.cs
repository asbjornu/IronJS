using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class NewArray : Node {
        public INode[] InitExpressions { get; private set; }

        public NewArray(SourcePosition pos)
            : this(pos, new INode[0]) {
        }

        public NewArray(SourcePosition pos, INode[] initExpressions)
            : base(pos) {
                InitExpressions = initExpressions;
        }
    }
}
