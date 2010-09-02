using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Identifier : Node {
        public string Name { get; protected set; }
        public int DynamicScopeLevel { get;      set; }

        Identifier(SourcePosition pos, string name, int dynamicScopeLevel)
            : base(pos) {
                Name = name;
                DynamicScopeLevel = dynamicScopeLevel;
        }

        public override INode Clone(INode[] children) {
            return this;
        }

        public static INode Create(SourcePosition pos, string name) {
            return Create(pos, name, 0);
        }

        public static INode Create(SourcePosition pos, string name, int dynamicScopeLevel) {
            return new Identifier(pos, name, dynamicScopeLevel);
        }
    }
}
