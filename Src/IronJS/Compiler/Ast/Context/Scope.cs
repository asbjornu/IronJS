using System;
using System.Linq;
using System.Collections.Generic;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class Scope {
        public VariableList Variables { get; private set; }

        public Scope() {
            Variables = new VariableList();
        }
    }
}
