using System;

namespace IronJS.Compiler.Ast.Context {
    public class Closure : INamed {

        public string Name {
            get;
            private set;
        }

        Closure(string name) {
            Name = name;
        }

        public static Closure Create(string name) {
            return new Closure(name);
        }
    }
}
