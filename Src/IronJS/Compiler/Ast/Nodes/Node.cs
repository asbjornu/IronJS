using System;

namespace IronJS.Compiler.Ast.Nodes {
    public abstract class Node : INode {
        public SourcePosition SourcePosition {
            get;
            private set;
        }

        public virtual Runtime.Type Type {
            get {
                return Runtime.Type.Dynamic;
            }
        }

        public virtual bool TypeResolved {
            get {
                return false;
            }
        }

        public Node(SourcePosition position) {
            SourcePosition = position;
        }

        public Node() {
            SourcePosition pos;

            pos.Column = -1;
            pos.Line = -1;

            SourcePosition = pos;
        }

        public bool As<T>(out T result) where T : class, INode {
            result = this as T;
            return result != null;
        }
    }
}
