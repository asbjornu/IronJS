using System;
using System.Linq;
using System.Collections.Generic;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class Scope {
        public NamedSet<Variable> Variables { get; private set; }
        public NamedSet<Closure> Closures { get; private set; }

        public Scope() {
            Variables = new NamedSet<Variable>();
            Closures = new NamedSet<Closure>();
        }
    }
}
