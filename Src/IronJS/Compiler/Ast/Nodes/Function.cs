using System;
using System.Linq;

namespace IronJS.Compiler.Ast.Nodes {
    public class Function : Node {
        public INode Body { get { return Children[0]; } }
        public Context.Scope Scope { get; private set; }

        public override Runtime.Type Type {
            get {
                return Runtime.Type.Function;
            }
        }

        public override bool TypeResolved {
            get {
                return true;
            }
        }

        public Function(SourcePosition pos, INode[] parameters, INode body)
            : base(pos) {
            Children = new[] { body };
            Scope = new Context.Scope();
            Scope.Variables.Add(parameters.Cast<Identifier>().Select(x => new Context.Variable(x.Name)));
        }

        public Function(SourcePosition pos, INode body, Context.Scope scope)
            : base(pos) {
            Children = new[] { body };
            Scope = scope;
        }
    }
}
