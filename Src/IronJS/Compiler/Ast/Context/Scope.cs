using System;
using System.Linq;
using System.Collections.Generic;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class Scope {
        public VariableSet Variables { get; private set; }

        public Scope() {
            Variables = new VariableSet();
        }

        public Scope Clone() {
            var scope = new Scope();
            scope.Variables = Variables.Clone();
            return scope;
        }
    }
}
