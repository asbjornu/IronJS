using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Identifier : Node {
        public string Name { get; private set; }

        public Identifier(SourcePosition position, string name)
            : base(position) {
                Name = name;
        }
    }
}
