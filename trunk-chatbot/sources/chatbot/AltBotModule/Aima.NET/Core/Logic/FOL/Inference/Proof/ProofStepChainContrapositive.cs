using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;

    public class ProofStepChainContrapositive : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Chain contrapositive = null;
        private Chain contrapositiveOf = null;

        public ProofStepChainContrapositive(Chain contrapositive, Chain contrapositiveOf) 
        {
            this.contrapositive = contrapositive;
            this.contrapositiveOf = contrapositiveOf;
            this.predecessors.Add(contrapositiveOf.ProofStep);
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return contrapositive.ToString();
        }

        public override string GetJustification() 
        {
            return "Contrapositive: " + contrapositiveOf.ProofStep.GetStepNumber();
        }
    }
}
