using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference.OTTER
{
    using Aima.Core.Logic.FOL.KB.Data;

    public interface IClauseFilter
    {
        ISet<Clause> Filter(ISet<Clause> clauses);
    }
}
