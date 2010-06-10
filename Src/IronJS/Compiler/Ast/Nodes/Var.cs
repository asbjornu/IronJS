using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr.Runtime.Tree;

namespace IronJS.Compiler.Ast.Nodes {
    public class Var : Node {
        public INode Node { get; private set; }

        public Var(SourcePosition position, INode node)
            : base(position) {
                Node = node;
        }
    }
}
