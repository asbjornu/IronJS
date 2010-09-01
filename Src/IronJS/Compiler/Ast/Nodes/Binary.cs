using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Binary : Node {
        public enum OpType {
            //Math
            Assign = 1, Add, Sub, Div, Mul, Mod, 
            
            //Other
            And, Or,

            //Logical
            Eq = 100, EqEq, Lt, Gt, GtEq, LtEq, NotEq
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

        Binary(SourcePosition pos, OpType op, INode[] children)
            : base(pos, children) {
                Op = op;
        }

        public override INode Clone(INode[] children) {
            return new Binary(Source, Op, children);
        }

        public static INode Create(SourcePosition pos, OpType op, INode left, INode right) {
            return new Binary(pos, op, new [] { left, right });
        }

        public static INode CreateAssign(SourcePosition pos, INode left, INode right) {
            return Create(pos, OpType.Assign, left, right);
        }
    }
}
