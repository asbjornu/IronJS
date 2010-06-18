using System;

namespace IronJS.Compiler.Ast.Nodes {
    public class Function : Node {
        public INode Body { get { return Children[0]; } }
        public Context.Scope Scope { get; private set; }

        public override Runtime.Type Type {
            get {
                return Runtime.Type.Function;
            }
        }

        public override bool TypeIsStatic {
            get {
                return true;
            }
        }

        public Function(SourcePosition pos, INode[] parameters, INode body)
            : base(pos) {

            Children = new[] { body };
            Scope = new Context.Scope();

            for (int i = 0; i < parameters.Length; ++i) {
                Scope.Variables.Add(new Context.Variable((parameters[i] as Identifier).Name, i));
            }
        }

        public Function(SourcePosition pos, INode body, Context.Scope scope)
            : base(pos) {
            Children = new[] { body };
            Scope = scope;
        }
    }
}
