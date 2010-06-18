using System;

namespace IronJS.Compiler.Ast {
    public interface INode {
        INode[] Children { get; set; }
        SourcePosition SourcePosition { get; }
        Runtime.Type Type { get; }
        bool TypeResolved { get; }
        bool HasChildren { get; }
    }
}
