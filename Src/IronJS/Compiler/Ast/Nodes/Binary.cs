using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public enum OpType {
            //Math
            Assign = 1, Add = 2, Sub = 3, Div = 4, Mul = 5, Mod = 6,

            And = 98, Or = 99,

            //Logical
            Eq = 103, EqEq = 104, Lt = 105, 
            Gt = 106, GtEq = 107, LtEq = 108, NotEq = 109
        }

        public OpType Op { get; protected set; }
        public INode Left { get { return Children[0]; } }
        public INode Right { get { return Children[1]; } }
        public bool IsAssign { get { return Op == OpType.Assign; } }

        public override bool TypeResolved {
            get {
                //Always know the type of logical operations
                if ((int)Op > 100)
                    return true;

                return Left.TypeResolved && Right.TypeResolved;
            }
        }

        public override Runtime.Type Type {
            get {
                //All logical operators have a value greater than 100
                if ((int)Op > 100)
                    return Runtime.Type.Boolean;

                return Left.Type | Right.Type;
            }
        }

        public Binary(SourcePosition position, OpType op, INode left, INode right)
            : base(position) {
                Op = op;
                Children = new[] { left, right };
        }

        public override INode Clone() {
            return new Binary(Source, Op, Left, Right);
        }
    }
}
