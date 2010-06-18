using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class New : Node {
        Runtime.Type _type;

        public INode Function { get { return Children[0]; } }
        public INode[] InitExpressions { get { return Children[0].Children; } }

        public override Runtime.Type Type {
            get {
                return _type;
            }
        }

        public override bool TypeIsStatic {
            get {
                return true;
            }
        }

        public New(SourcePosition pos, Runtime.Type type)
            : this(pos, type, new INode[0]) {
        }

        public New(SourcePosition pos, Runtime.Type type, INode[] initExpressions)
            : base(pos) {
                _type = type;
                Children = new[] { null, new Node(initExpressions) };
        }

        public New(SourcePosition pos, INode function)
            : base(pos) {
                _type = Runtime.Type.Object;
                Children = new INode[] { function, new Node(new Node[0]) };
        }

        public New(SourcePosition pos, Runtime.Type type, INode function, INode[] initExpressions)
            : base(pos) {
                _type = type;
                Children = new INode[] { function, new Node(initExpressions) };
        }
    }
}
