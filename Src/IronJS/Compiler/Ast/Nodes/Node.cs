using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Node : INode {
        public INode[] Children { get; set; }
        public SourcePosition SourcePosition { get; private set; }

        public virtual Runtime.Type Type {
            get { return Runtime.Type.Dynamic; }
        }

        public virtual bool TypeResolved {
            get { return false; }
        }

        public bool HasChildren {
            get { return Children != null && Children.Length > 0; }
        }

        public Node(SourcePosition position) {
            SourcePosition = position;
        }
        public Node(INode[] children) {
            Children = children;
            SourcePosition = SourcePosition.Unknown;
        }

        public Node() {
            SourcePosition = SourcePosition.Unknown;
        }
    }
}
