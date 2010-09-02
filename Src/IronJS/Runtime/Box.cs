using System.Runtime.InteropServices;

namespace IronJS.Runtime {
    [StructLayout(LayoutKind.Explicit)]
    public struct Box {
        [FieldOffset(0)] object Clr;
        [FieldOffset(8)] bool Bool;
        [FieldOffset(8)] double Double;
        [FieldOffset(16)] Type Type;
    }
}
