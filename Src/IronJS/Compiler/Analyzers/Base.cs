using System.Collections.Generic;
using System.Linq;
using IronJS.Compiler.Ast;
using IronJS.Compiler.Ast.Context;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Analyzers {
    public abstract class Base : IAnalyzer {
        protected Scope Scope {
            get {
                return ScopeChain.Peek();
            }
        }

        protected Stack<Scope> ScopeChain {
            get;
            private set;
        }

        public Base() {
            ScopeChain = new Stack<Scope>();
        }

        public INode[] Analyze(Scope scope, INode[] nodes) {
            ScopeChain.Clear();
            ScopeChain.Push(scope);
            return nodes.Select(x => Analyze(x)).ToArray();
        }

        protected INode AnalyzeChildren(INode node) {
            node.Children = node.Children.Select(x => Analyze(x)).ToArray();
            return node;
        }

        protected INode Analyze(Function node) {
            ScopeChain.Push(node.Scope);
            AnalyzeChildren(node);
            ScopeChain.Pop();
            return node;
        }

        protected abstract INode Analyze(INode node);
    }
}
