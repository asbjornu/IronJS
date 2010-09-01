using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Node : INode {
        public INode[] Children {
            get;
            private set;
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

        protected Node(SourcePosition position, INode[] children) {
            Source = position;
            Children = children;
        }

        protected Node(SourcePosition position)
            : this(position, new INode[0]) {
        }

        protected Node(INode[] children)
            : this(SourcePosition.Unknown, children) {
        }

        public bool As<T>(out T result) where T : class, INode {
            result = this as T;
            return result != null;
        }

        public virtual INode Clone(INode[] children) {
            return new Node(Source, children);
        }

        public static INode Create() {
            return new Node(SourcePosition.Unknown);
        }

        public static INode Create(SourcePosition pos) {
            return new Node(pos);
        }

        public static INode Create(INode[] children) {
            return new Node(children);
        }

        public static INode Create(SourcePosition pos, INode[] children) {
            return new Node(pos, children);
        }
    }
}
