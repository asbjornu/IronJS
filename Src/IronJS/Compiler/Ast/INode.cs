using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS.Compiler.Ast {
    public interface INode {
        SourcePosition SourcePosition { get; }
    }
}
