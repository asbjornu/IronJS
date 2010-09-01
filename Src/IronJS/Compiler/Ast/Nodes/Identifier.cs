using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Identifier : Node {
        public string Name { get; protected set; }

        Identifier(SourcePosition pos, string name)
            : base(pos, new INode[0]) {
                Name = name;
        }

        public override INode Clone(INode[] children) {
            return this;
        }

        public static INode Create(SourcePosition pos, string name) {
            return new Identifier(pos, name);
        }
    }
}
