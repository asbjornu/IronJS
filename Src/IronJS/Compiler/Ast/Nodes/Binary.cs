using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public BinaryOp Op { get; protected set; }
        public INode Left { get { return Children[0]; } }
        public INode Right { get { return Children[1]; } }
        public bool IsAssign { get { return Op == BinaryOp.Assign; } }

        public override bool TypeIsStatic {
            get {
                return Left.TypeIsStatic && Right.TypeIsStatic;
            }
        }

        public override Runtime.Type Type {
            get {
                //All logical operators have a value equal to or above 100
                if ((int)Op >= 100)
                    return Runtime.Type.Boolean;

                return Left.Type | Right.Type;
            }
        }

        public Binary(SourcePosition position, BinaryOp op, INode left, INode right)
            : base(position) {
                Op = op;
                Children = new[] { left, right };
        }
    }
}
