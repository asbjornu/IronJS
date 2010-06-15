using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Literal<T> : Node {
        public T Value { get; private set; }

        public Literal(SourcePosition position, T value)
            : base(position) {
                Value = value;
        }
    }

    public static class Literal {
        public static INode Create<T>(SourcePosition pos, T value) {
            return new Literal<T>(pos, value) as INode;
        }

        public static INode True {
            get {
                return Literal.Create(SourcePosition.Unknown, true);
            }
        }

        public static INode False {
            get {
                return Literal.Create(SourcePosition.Unknown, false);
            }
        }
    }
}
