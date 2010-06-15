using System;

namespace IronJS.Compiler.Ast {
    public struct SourcePosition {
        public int Line;
        public int Column;

        public static SourcePosition Unknown {
            get {
                SourcePosition pos;

                pos.Column = -1;
                pos.Line = -1;

                return pos;
            }
        }
    }
}
