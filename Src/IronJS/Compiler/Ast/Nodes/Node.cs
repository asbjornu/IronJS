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
