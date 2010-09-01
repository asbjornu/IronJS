using System;
using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;
using System.Collections.Generic;

namespace IronJS.Compiler.Analyzer {
    public class Default : IAnalyzer {
        Stack<Scope> _scopes;

        Scope Scope {
            get { return _scopes.Peek(); }
        }

        public Default() {
            _scopes = new Stack<Scope>();
        }

        public INode[] Analyze(Scope scope, INode[] nodes) {
            _scopes.Clear();
            _scopes.Push(scope);

            var analyzedNodes = new INode[nodes.Length];

            for(var i = 0; i < nodes.Length; ++i) {
                analyzedNodes[i] = Analyze(nodes[i]);
            }

            return analyzedNodes;
        }

        INode Analyze(INode node) {
            if (node == null) {
                return null;

            } else if (node is Var) {
                return Analyze(node as Var);

            } else if (node is Binary) {
                return Analyze(node as Binary);

            } else if (node is Function) {
                return Analyze(node as Function);

            } else if (node is Unary) {
                return Analyze(node as Unary);

            } else {
                if(node.HasChildren) {
                    node.Children = node.Children.Select(x => Analyze(x)).ToArray();
                }

                return node;
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
                        Scope.Variables.Get(target).AddType(Runtime.Type.Double);
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
                        Scope.Variables.Get(left).AddAssignedFrom(right);
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
                    Scope.Variables.Add(new Variable(identifier.Name));
                    return Analyze(binary);
                }
                throw new Error();

            } else if (node.Node.As<Identifier>(out identifier)) {
                Scope.Variables.Add(new Variable(identifier.Name));
                return new Pass();

            }

            throw new Error();
        }
    }
}
