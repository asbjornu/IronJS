using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Nodes {
    public class ForStep : Node {
        public INode Init { get; private set; }
        public INode Test { get; private set; }
        public INode Incr { get; private set; }
        public INode Body { get; private set; }

        public ForStep(SourcePosition pos, INode init, INode test, INode incr, INode body)
            : base(pos) {
                Init = init;
                Test = test;
                Incr = incr;
                Body = body;
        }
    }
}
