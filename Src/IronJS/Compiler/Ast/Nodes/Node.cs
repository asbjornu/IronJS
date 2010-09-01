using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Node : INode {
        public INode[] Children {
            get;
            set;
        }

        public SourcePosition Source {
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

        public bool HasChildren {
            get {
                return Children != null && Children.Length > 0;
            }
        }

        public Node(INode[] children, SourcePosition position)
            : this(position) {
                Children = children;
        }

        public Node(SourcePosition position) {
            Source = position;
        }

        public Node(INode[] children) {
            Children = children;
            Source = SourcePosition.Unknown;
        }

        public Node() {
            Source = SourcePosition.Unknown;
        }

        public bool As<T>(out T result) where T : class, INode {
            result = this as T;
            return result != null;
        }
    }
}
