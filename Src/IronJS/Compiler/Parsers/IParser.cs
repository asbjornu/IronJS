using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Parsers {
    public interface IParser {
        INode[] ParseFile(string fileName);
        INode[] ParseSource(string source);
    }
}
