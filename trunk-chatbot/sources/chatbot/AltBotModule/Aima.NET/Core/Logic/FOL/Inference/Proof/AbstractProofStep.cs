using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    public abstract class AbstractProofStep : IProofStep 
    {
        private int step = 0;

        public int GetStepNumber() 
        {
            return step;
        }

        public void SetStepNumber(int step) 
        {
            this.step = step;
        }

        public abstract IList<IProofStep> GetPredecessorSteps();

        public abstract string GetProof();

        public abstract string GetJustification();

    }
}
