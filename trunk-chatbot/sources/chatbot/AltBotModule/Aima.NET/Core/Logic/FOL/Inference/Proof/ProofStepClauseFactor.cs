using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;

    public class ProofStepClauseFactor : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause factor = null;
        private Clause factorOf = null;

        public ProofStepClauseFactor(Clause factor, Clause factorOf) 
        {
            this.factor = factor;
            this.factorOf = factorOf;
            this.predecessors.Add(factorOf.GetProofStep());
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return factor.ToString();
        }

        public override string GetJustification() 
        {
            return "Factor of " + factorOf.GetProofStep().GetStepNumber();
        }
    }
}
