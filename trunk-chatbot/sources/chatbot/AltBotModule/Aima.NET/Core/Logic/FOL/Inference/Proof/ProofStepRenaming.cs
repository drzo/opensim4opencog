using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    public class ProofStepRenaming : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private object proof = "";

        public ProofStepRenaming(object proof, IProofStep predecessor) 
        {
            this.proof = proof;
            this.predecessors.Add(predecessor);
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return proof.ToString();
        }

        public override string GetJustification() 
        {
            return "Renaming of " + predecessors[0].GetStepNumber();
        }
    }
}
