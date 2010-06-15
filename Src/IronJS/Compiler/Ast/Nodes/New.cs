using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class New : Node {
        public Runtime.Types Type { get; private set; }
        public INode Function { get; private set; }
        public Tuple<INode, INode>[] InitExpressions { get; private set; }

        public New(SourcePosition pos, Runtime.Types type)
            : this(pos, type, new Tuple<INode, INode>[0]) {
        }

        public New(SourcePosition pos, Runtime.Types type, Tuple<INode, INode>[] initExpressions)
            : base(pos) {
                Type = type;
                InitExpressions = initExpressions;
        }

        public New(SourcePosition pos, INode function)
            : base(pos) {
                Function = function;
                Type = Runtime.Types.Object;
                InitExpressions = new Tuple<INode, INode>[0];
        }
    }
}
