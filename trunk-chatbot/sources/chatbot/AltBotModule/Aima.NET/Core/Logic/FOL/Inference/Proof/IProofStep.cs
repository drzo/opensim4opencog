using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    public interface IProofStep
    {
        int GetStepNumber();

        void SetStepNumber(int step);

        IList<IProofStep> GetPredecessorSteps();

        string GetProof();

        string GetJustification();
    }
}
