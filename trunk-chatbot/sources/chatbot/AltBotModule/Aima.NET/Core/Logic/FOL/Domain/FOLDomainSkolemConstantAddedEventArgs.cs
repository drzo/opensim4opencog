using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Domain
{
    public class FOLDomainSkolemConstantAddedEventArgs : EventArgs 
    {
        public string SkolemConstantName { get; private set; }

        public FOLDomainSkolemConstantAddedEventArgs(string skolemConstantName) 
        {
            this.SkolemConstantName = skolemConstantName;
        }
    }
}
