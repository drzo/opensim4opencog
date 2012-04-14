using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;

    public class ProofStepFoChAlreadyAFact : AbstractProofStep 
    {
        private static readonly IList<IProofStep> NoPredecessors = new List<IProofStep>();

        private Literal fact = null;

        public ProofStepFoChAlreadyAFact(Literal fact) {
            this.fact = fact;
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(NoPredecessors);
        }

        public override string GetProof() {
            return fact.ToString();
        }

        public override string GetJustification() {
            return "Already a known fact in the KB.";
        }
    }
}
