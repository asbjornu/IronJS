using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Node : INode {
        public INode[] Children { get; set; }
        public SourcePosition Source { get; private set; }

        public virtual Runtime.Type Type {
            get { return Runtime.Type.Dynamic; }
        }

        public virtual bool TypeIsStatic {
            get { return false; }
        }

        public bool HasChildren {
            get { return Children != null && Children.Length > 0; }
        }

        public bool As<T>(out T result) where T : class {
            if (this is T) {
                result = this as T;
                return true;
            }

            result = null;
            return false;
            //throw new Error("Expecting Node of type '{0}' but got '{1}'", typeof(T).Name, this.GetType().Name);
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
    }
}
