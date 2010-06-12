using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Nodes {
    public class Invoke : Node {
        public INode Target { get; private set; }
        public INode[] Arguments { get; private set; }

        public Invoke(SourcePosition pos, INode target, INode[] arguments)
            : base(pos) {
                Target = target;
                Arguments = arguments;
        }
    }
}
