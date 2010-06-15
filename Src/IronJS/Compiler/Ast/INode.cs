using System;

namespace IronJS.Compiler.Ast {
    public interface INode {
        SourcePosition SourcePosition { get; }
    }
}
