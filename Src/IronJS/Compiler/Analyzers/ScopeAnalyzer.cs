using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class ScopeAnalyzer : Base {
        protected override INode AnalyzeNode(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Function) {
                return AnalyzeFunction(node as Function);

            } else if (node is Identifier) {
                return AnalyzeIdentifier(node as Identifier);

            } else {
                return AnalyzeChildren(node);
            }
        }

        INode AnalyzeIdentifier(Identifier node) {
            if (!Scope.Variables.Has(node.Name)) {
                if (!Scope.Closures.Has(node.Name)) {
                    var scopes = ScopeChain.Reverse().SkipWhile(x => !x.Variables.Has(node.Name));
                    if (scopes.Count() > 0) {
                        scopes.First().Variables.Get(node.Name).IsClosedOver = true;
                        foreach (var scope in scopes.Skip(1)) {
                            if (!scope.Closures.Has(node.Name)) {
                                scope.Closures.Add(Closure.Create(node.Name));
                            }
                        }
                    }
                }
            }

            return node;
        }

    }
}
