using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class VariableSet {
        HashSet<Variable> _variables;

        class VariableComparer : IEqualityComparer<Variable> {
            public bool Equals(Variable x, Variable y) { return x.Name == y.Name; }
            public int GetHashCode(Variable obj) { return obj.Name.GetHashCode(); }
        }

        public VariableSet() {
            _variables = new HashSet<Variable>(new VariableComparer());
        }

        public void Add(Variable var) {
            _variables.Add(var);
        }

        public Variable Get(string name) {
            return _variables.First(x => x.Name == name);
        }

        public Variable Get(INode node) {
            if (node is Identifier) {
                return _variables.First(x => x.Name == (node as Identifier).Name);
            }

            throw new Exception();
        }
    }
}
