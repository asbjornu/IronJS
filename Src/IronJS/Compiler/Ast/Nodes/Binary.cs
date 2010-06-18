using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public BinaryOp Op { get; protected set; }
        public INode Left { get { return Children[0]; } }
        public INode Right { get { return Children[1]; } }

        public override bool TypeResolved {
            get {
                return Left.TypeResolved && Right.TypeResolved;
            }
        }

        public Binary(SourcePosition position, BinaryOp op, INode left, INode right)
            : base(position) {
                Op = op;
                Children = new[] { left, right };
        }
    }
}
