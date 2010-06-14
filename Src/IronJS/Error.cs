using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IronJS {
    public class Error : Exception {
        public Error(string message, params object[] args)
            : base(String.Format(message, args)) {
        }
    }
}
