using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class For : Node {
        public INode Init { get; private set; }
        public INode Test { get; private set; }
        public INode Incr { get; private set; }
        public INode Body { get; private set; }

        public For(SourcePosition pos, INode init, INode test, INode incr, INode body)
            : base(pos) {
                Init = init;
                Test = test;
                Incr = incr;
                Body = body;
        }
    }
}
