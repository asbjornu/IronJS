using System;

namespace IronJS.Compiler.Ast.Nodes {
    public sealed class Pass : Node {
        static INode _instance;

        Pass(SourcePosition pos, INode[] children)
            : base(pos, children) {

        }

        public static INode Instance {
            get {
                if (_instance == null) {
                    _instance = new Pass(SourcePosition.Unknown, new INode[0]);
                }

                return _instance;
            }
        }

        public override INode Clone(INode[] children) {
            return this;
        }
    }
}
