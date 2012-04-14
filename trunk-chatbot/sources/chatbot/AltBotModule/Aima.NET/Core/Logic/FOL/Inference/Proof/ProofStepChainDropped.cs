using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;

    public class ProofStepChainDropped : AbstractProofStep {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Chain dropped = null;
        private Chain droppedOff = null;

        public ProofStepChainDropped(Chain dropped, Chain droppedOff) {
            this.dropped = dropped;
            this.droppedOff = droppedOff;
            this.predecessors.Add(droppedOff.ProofStep);
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            return dropped.ToString();
        }

        public override string GetJustification() {
            return "Dropped: " + droppedOff.ProofStep.GetStepNumber();
        }
    }
}
