using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Trace
{
    public interface IFOLModelEliminationTracer
    {
        void Reset();

        void Increment(int depth, int noFarParents);
    }
}
