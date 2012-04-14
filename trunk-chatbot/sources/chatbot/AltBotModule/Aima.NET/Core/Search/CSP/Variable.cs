using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// A variable is a distinguishable object with a name.
    /// </summary>
    public class Variable
    {
        public string Name { get; private set; }

        public Variable(string name)
        {
            this.Name = name;
        }

        public override string ToString()
        {
            return this.Name;
        }
    }
}
