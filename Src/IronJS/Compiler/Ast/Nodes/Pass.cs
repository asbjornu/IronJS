using System;

namespace IronJS.Compiler.Ast.Nodes {
    public sealed class Pass : Node {
        static INode _instance;

        Pass() {

        }

        public static INode Instance {
            get {
                if (_instance == null) {
                    _instance = new Pass();
                }

                return _instance;
            }
        }

        public override INode Clone() {
            return this;
        }
    }
}
