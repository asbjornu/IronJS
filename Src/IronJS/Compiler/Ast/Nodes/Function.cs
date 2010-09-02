using System;
using System.Linq;

namespace IronJS.Compiler.Ast.Nodes {
    public class Function : Node {
        public INode Body { get { return Children[0]; } }
        public Context.Scope Scope { get; protected set; }

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

        Function(SourcePosition pos, INode[] children, Context.Scope scope)
            : base(pos, children) {
                Scope = scope;
        }

        public override INode Clone(INode[] children) {
            return new Function(Source, children, Scope);
        }

        public static INode Create(SourcePosition pos, INode[] parameters, INode body) {
            var index = 0;
            var node = new Function(pos, new[] { body }, new Context.Scope());
            node.Scope.Variables.Add(parameters.Cast<Identifier>().Select(x => Context.Variable.CreateParameter(x.Name, ++index) ));
            return node;
        }

        public static INode Create(SourcePosition pos, INode body) {
            return Create(pos, new INode[0], body);
        }

        public static new INode Create(SourcePosition pos, INode[] nodes) {
            return Create(pos, Block.Create(pos, nodes));
        }

        public static INode CreateWithScope(SourcePosition pos, INode body, Context.Scope scope) {
            return new Function(pos, new[] { body }, new Context.Scope());
        }
    }
}
