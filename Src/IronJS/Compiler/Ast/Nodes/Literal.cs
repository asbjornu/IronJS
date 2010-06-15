using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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
    }
}
