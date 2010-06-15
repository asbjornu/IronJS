using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public BinaryOp Op { get; protected set; }
        public INode Left { get; private set; }
        public INode Right { get; private set; }

        public override bool TypeResolved {
            get {
                return Left.TypeResolved && Right.TypeResolved;
            }
        }

        public Binary(SourcePosition position, BinaryOp op, INode left, INode right)
            : base(position) {
                Op = op;
                Left = left;
                Right = right;
        }
    }
}
