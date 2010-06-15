using System;
using System.Collections.Generic;

namespace IronJS.Compiler.Ast.Context {
    public class Scope {
        public List<Variable> Variables { get; private set; }

        public Scope() {
            Variables = new List<Variable>();
        }
    }
}
