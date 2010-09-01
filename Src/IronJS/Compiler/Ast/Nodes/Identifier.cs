using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Identifier : Node {
        public string Name { get; protected set; }

        public Identifier(SourcePosition position, string name)
            : base(position) {
                Name = name;
        }

        public override INode Clone() {
            return this;
        }
    }
}
