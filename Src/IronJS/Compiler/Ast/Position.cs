using System;

namespace IronJS.Compiler.Ast {
    public struct SourcePosition {
        public readonly int Line;
        public readonly int Column;

        public SourcePosition(int line, int column) {
            Line = line;
            Column = column;
        }

        public static SourcePosition Unknown {
            get {
                return new SourcePosition(-1, -1);
            }
        }
    }
}
