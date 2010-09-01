using System.Collections.Generic;
using System.Linq;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class VariableSet {
        HashSet<Variable> _variables;

        public VariableSet() {
            _variables = new HashSet<Variable>(new VariableComparer());
        }

        public void Add(Variable var) {
            _variables.Add(var);
        }

        public void Add(IEnumerable<Variable> vars) {
            _variables.UnionWith(vars);
        }

        public Variable Get(string name) {
            return _variables.First(x => x.Name == name);
        }

        public Variable Get(INode node) {
            return Get((node as Identifier).Name);
        }

        class VariableComparer : IEqualityComparer<Variable> {
            public bool Equals(Variable x, Variable y) { return x.Name == y.Name; }
            public int GetHashCode(Variable obj) { return obj.Name.GetHashCode(); }
        }
    }
}
