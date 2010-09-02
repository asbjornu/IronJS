using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class New : Node {
        Runtime.Type _type;

        public INode FunctionExpression { get { return Children[0]; } }
        public INode[] InitExpressions { get { return Children[0].Children; } }

        public override Runtime.Type Type {
            get {
                return _type;
            }
        }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        New(SourcePosition pos, INode[] children, Runtime.Type type)
            : base(pos, children) {
            _type = type;
        }

        public static INode Object(SourcePosition pos, INode[] initExpressions) {
            return new New(pos, new[] { null, Node.Create(initExpressions) }, Runtime.Type.Object);
        }

        public static INode Object(SourcePosition pos, INode function, INode[] initExpressions) {
            return new New(pos, new[] { function, Node.Create(initExpressions) }, Runtime.Type.Object);
        }

        public static INode Array(SourcePosition pos, INode[] initExpressions) {
            return new New(pos, new[] { null, Node.Create(initExpressions) }, Runtime.Type.Array);
        }

        public override INode Clone(INode[] children) {
            return new New(Source, children, _type);
        }
    }
}
