using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public BinaryOp BinaryOp { get; protected set; }
        public INode Left { get; private set; }
        public INode Right { get; private set; }

        public Binary(SourcePosition position, BinaryOp binaryOp, INode left, INode right)
            : base(position) {
                BinaryOp = binaryOp;
                Left = left;
                Right = right;
        }
    }
}
