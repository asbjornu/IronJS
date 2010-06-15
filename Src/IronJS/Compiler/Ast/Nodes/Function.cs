using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Function : Node {
        public INode[] Parameters { get; private set; }
        public INode Body { get; private set; }

        public Function(SourcePosition pos, INode[] parameters, INode body)
            : base(pos) {
                Parameters = parameters;
                Body = body;
        }
    }
}
