using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Literal<T> : Node {
        public T Value { get; private set; }

        public override Runtime.Type Type {
            get {
                return Runtime.TypeConverter.ClrToJs<T>();
            }
        }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        Literal(SourcePosition position, T value)
            : base(position) {
                Value = value;
        }

        public override INode Clone(INode[] children) {
            return this;
        }

        public static INode Create(SourcePosition pos, T value) {
            return new Literal<T>(pos, value);
        }
    }

    public static class Literal {
        public static INode Create<T>(SourcePosition pos, T value) {
            return Literal<T>.Create(pos, value);
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
