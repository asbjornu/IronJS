using IronJS.Compiler.Ast;

namespace IronJS.Compiler.Parser {
    public interface IParser {
        INode[] ParseFile(string fileName);
        INode[] ParseSource(string source);
    }
}
