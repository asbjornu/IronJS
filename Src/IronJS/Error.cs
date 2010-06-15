using System;

namespace IronJS {
    public class Error : Exception {
        public Error(string message, params object[] args)
            : base(String.Format(message, args)) {
        }
    }
}
