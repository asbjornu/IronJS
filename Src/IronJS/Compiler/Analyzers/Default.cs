using System.Collections.Generic;
using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class Default : Base {
        protected override INode Analyze(INode node) {
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
                return AnalyzeChildrenAndClone(node);
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

            return Unary.Create(node.Source, target, node.Op);
        }

        INode Analyze(Function node) {
            ScopeChain.Push(node.Scope);
            return Function.CreateWithScope(node.Source, Analyze(node.Body), ScopeChain.Pop());
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

            return Binary.Create(node.Source, node.Op, left, right);
        }

        INode Analyze(Var node) {
            Binary binary;
            Identifier identifier;

            if (node.Node.As<Binary>(out binary) && binary.IsAssign) {
                if (binary.Left.As<Identifier>(out identifier)) {
                    Scope.Variables.Add(new Variable(identifier.Name));
                    return Analyze(binary);
                }

                throw new CompilerError();

            } else if (node.Node.As<Identifier>(out identifier)) {
                Scope.Variables.Add(new Variable(identifier.Name));
                return Pass.Instance;
                    
            }

            throw new CompilerError();
        }
    }
}
