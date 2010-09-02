using System;

namespace IronJS.Compiler.Ast.Context {
    public class Closure : INamed {
        public int ScopeLevel {
            get;
            private set;
        }

        public string Name {
            get;
            private set;
        }

        Closure(string name, int scopeLevel) {
            Name = name;
            ScopeLevel = scopeLevel;
        }

        public static Closure Create(string name, int scopeLevel) {
            return new Closure(name, scopeLevel);
        }
    }
}
