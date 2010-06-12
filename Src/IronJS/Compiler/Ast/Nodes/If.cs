using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Nodes {
    public class If : Node {
        public INode Test { get; private set; }
        public INode IfTrue { get; private set; }
        public INode IfFalse { get; private set; }

        public If(SourcePosition position, INode test, INode ifTrue, INode ifFalse)
            : base(position) {
                Test = test;
                IfTrue = ifTrue;
                IfFalse = ifFalse;
        }
    }
}
