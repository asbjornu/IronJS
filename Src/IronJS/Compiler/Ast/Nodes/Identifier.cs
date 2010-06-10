using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast.Nodes {
    public class Identifier : Node {
        public string Name { get; private set; }

        public Identifier(SourcePosition position, string name)
            : base(position) {
                Name = name;
        }
    }
}
