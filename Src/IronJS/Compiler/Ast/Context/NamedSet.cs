using System.Collections.Generic;
using System.Linq;
using IronJS.Compiler.Ast.Nodes;

namespace IronJS.Compiler.Ast.Context {
    public class NamedSet<T> where T : INamed {
        HashSet<T> _set;

        public NamedSet() {
            _set = new HashSet<T>(new VariableComparer());
        }

        public void Add(T var) {
            _set.Add(var);
        }

        public void Add(IEnumerable<T> vars) {
            _set.UnionWith(vars);
        }

        public T Get(string name) {
            return _set.First(x => x.Name == name);
        }

        public T Get(INode node) {
            return Get((node as Identifier).Name);
        }

        public bool Has(string name) {
            return _set.Count(x => x.Name == name) > 0;
        }

        class VariableComparer : IEqualityComparer<T> {
            public bool Equals(T x, T y) { return x.Name == y.Name; }
            public int GetHashCode(T obj) { return obj.Name.GetHashCode(); }
        }
    }
}
