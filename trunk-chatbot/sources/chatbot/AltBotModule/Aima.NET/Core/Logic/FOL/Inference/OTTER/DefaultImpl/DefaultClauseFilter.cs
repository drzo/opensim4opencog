using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference.OTTER.DefaultImpl
{
    using Aima.Core.Logic.FOL.KB.Data;

    public class DefaultClauseFilter : IClauseFilter 
    {
        public ISet<Clause> Filter(Iesi.Collections.Generic.ISet<Clause> clauses) 
        {
            return clauses;
        }
    }
}
