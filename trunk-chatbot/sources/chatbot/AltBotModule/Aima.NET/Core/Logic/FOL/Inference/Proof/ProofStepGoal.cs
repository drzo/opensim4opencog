using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    public class ProofStepGoal : AbstractProofStep 
    {

        private static readonly IList<IProofStep> _noPredecessors = new List<IProofStep>();

        private object proof = "";

        public ProofStepGoal(object proof) 
        {
            this.proof = proof;
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(_noPredecessors);
        }

        public override string GetProof() 
        {
            return proof.ToString();
        }

        public override string GetJustification() 
        {
            return "Goal";
        }
    }
}
