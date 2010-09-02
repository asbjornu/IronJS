using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public class ScopeAnalyzer : Base {
        int _dynamicScopeLevel = 0;

        protected override INode AnalyzeNode(INode node) {
            if (node == null) {
                return Pass.Instance;

            } else if (node is Function) {
                return AnalyzeFunction(node as Function);

            } else if (node is Identifier) {
                return AnalyzeIdentifier(node as Identifier);

            } else if (node is With) {
                return AnalyzeWith(node as With);

            } else {
                return AnalyzeChildren(node);
            }
        }

        INode AnalyzeWith(With node) {
            ++_dynamicScopeLevel;
            var analyzed = AnalyzeChildren(node);
            --_dynamicScopeLevel;
                return analyzed;
        }

        INode AnalyzeIdentifier(Identifier node) {
            if (!Scope.Variables.Has(node.Name)) {
                if (!Scope.Closures.Has(node.Name)) {
                    var scopes = ScopeChain.Reverse().SkipWhile(x => !x.Variables.Has(node.Name));
                    if (scopes.Count() > 0) {
                        scopes.First().Variables.Get(node.Name).IsClosedOver = true;
                        var scopeLevel = ScopeChain.Count() - scopes.Count();
                        foreach (var scope in scopes.Skip(1)) {
                            if (!scope.Closures.Has(node.Name)) {
                                scope.Closures.Add(Closure.Create(node.Name, scopeLevel));
                            }
                        }
                    }
                }
            }

            return Identifier.Create(node.Source, node.Name, _dynamicScopeLevel);
        }
    }
}
