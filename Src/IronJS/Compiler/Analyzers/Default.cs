using System.Collections.Generic;
using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class Default : IAnalyzer {
        Stack<Scope> _scopes;

        Scope _scope {
            get {
                return _scopes.Peek();
            }
        }

        public Default() {
            _scopes = new Stack<Scope>();
        }

        public INode[] Analyze(Scope scope, INode[] nodes) {
            _scopes.Clear();
            _scopes.Push(scope);
            return nodes.Select(x => Analyze(x)).ToArray();
        }

        INode Analyze(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Var) {
                return Analyze(node as Var);

            } else if (node is Binary) {
                return Analyze(node as Binary);

            } else if (node is Function) {
                return Analyze(node as Function);

            } else if (node is Unary) {
                return Analyze(node as Unary);

            } else {
                var clone = node.Clone();

                if (clone.HasChildren) {
                    clone.Children = clone.Children.Select(x => Analyze(x)).ToArray();
                }

                return clone;
            }
        }

        INode Analyze(Unary node) {
            var target = Analyze(node.Target);

            switch (node.Op) {
                case Unary.OpType.Inc:
                case Unary.OpType.Dec:
                case Unary.OpType.PostDec:
                case Unary.OpType.PostInc:
                    if (target is Identifier) {
                        _scope.Variables.Get(target).AddType(Runtime.Type.Double);
                    }
                    break;
            }

            return new Unary(node.Source, target, node.Op);
        }

        INode Analyze(Function node) {
            _scopes.Push(node.Scope);
            return new Function(node.Source, Analyze(node.Body), _scopes.Pop());
        }

        INode Analyze(Binary node) {
            var left = Analyze(node.Left);
            var right = Analyze(node.Right);

            switch (node.Op) {
                case Binary.OpType.Assign:
                    if (left is Identifier) {
                        _scope.Variables.Get(left).AddAssignedFrom(right);
                    }
                    break;
            }

            return new Binary(node.Source, node.Op, left, right);
        }

        INode Analyze(Var node) {
            Binary binary;
            Identifier identifier;

            if (node.Node.As<Binary>(out binary) && binary.IsAssign) {
                if (binary.Left.As<Identifier>(out identifier)) {
                    _scope.Variables.Add(new Variable(identifier.Name));
                    return Analyze(binary);
                }

                throw new CompilerError();

            } else if (node.Node.As<Identifier>(out identifier)) {
                _scope.Variables.Add(new Variable(identifier.Name));
                return Pass.Instance;
                    
            }

            throw new CompilerError();
        }
    }
}
