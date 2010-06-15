using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class New : Node {
        Runtime.Type _type;

        public INode Function { get; private set; }
        public Tuple<INode, INode>[] InitExpressions { get; private set; }

        public override Runtime.Type Type {
            get {
                return _type;
            }
        }

        public override bool StaticType {
            get {
                return true;
            }
        }

        public New(SourcePosition pos, Runtime.Type type)
            : this(pos, type, new Tuple<INode, INode>[0]) {
        }

        public New(SourcePosition pos, Runtime.Type type, Tuple<INode, INode>[] initExpressions)
            : base(pos) {
                _type = type;
                InitExpressions = initExpressions;
        }

        public New(SourcePosition pos, INode function)
            : base(pos) {
                Function = function;
                _type = Runtime.Type.Object;
                InitExpressions = new Tuple<INode, INode>[0];
        }
    }
}
