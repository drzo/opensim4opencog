using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference.Trace
{
    using Aima.Core.Logic.FOL.KB.Data;

    public interface IFOLTFMResolutionTracer
    {
        void StepStartWhile(Iesi.Collections.Generic.ISet<Clause> clauses, int totalNoClauses,
                int totalNoNewCandidateClauses);

        void StepOuterFor(Clause i);

        void StepInnerFor(Clause i, Clause j);

        void StepResolved(Clause iFactor, Clause jFactor, Iesi.Collections.Generic.ISet<Clause> resolvents);

        void StepFinished(Iesi.Collections.Generic.ISet<Clause> clauses, IInferenceResult result);
    }
}
