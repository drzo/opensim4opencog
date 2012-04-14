using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.OTTER
{
    using Aima.Core.Logic.FOL.KB.Data;

    public interface IClauseSimplifier
    {
        Clause Simplify(Clause c);
    }
}
