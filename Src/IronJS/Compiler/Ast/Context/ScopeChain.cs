using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Context {
    public class ScopeChain {
        Stack<Scope> _scopes;

        public Scope Current {
            get { return _scopes.Peek(); }
        }

        public ScopeChain() {
            _scopes = new Stack<Scope>();
        }

        public void Enter(Scope scope) {
            _scopes.Push(scope);
        }

        public Scope Exit() {
            return _scopes.Pop();
        }
    }
}
