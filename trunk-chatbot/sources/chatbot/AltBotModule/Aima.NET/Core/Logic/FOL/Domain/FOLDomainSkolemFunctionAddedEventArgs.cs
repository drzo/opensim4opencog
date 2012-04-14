using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Domain
{
    public class FOLDomainSkolemFunctionAddedEventArgs : EventArgs
    {
        public string SkolemFunctionName { get; private set; }

        public FOLDomainSkolemFunctionAddedEventArgs(string skolemFunctionName) 
        {
            this.SkolemFunctionName = skolemFunctionName;
        }
    }
}
