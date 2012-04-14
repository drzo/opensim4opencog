using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepChainCancellation : AbstractProofStep {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Chain cancellation = null;
        private Chain cancellationOf = null;
        private IDictionary<Variable, ITerm> subst = null;

        public ProofStepChainCancellation(Chain cancellation, Chain cancellationOf,
                IDictionary<Variable, ITerm> subst) {
            this.cancellation = cancellation;
            this.cancellationOf = cancellationOf;
            this.subst = subst;
            this.predecessors.Add(cancellationOf.ProofStep);
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            return cancellation.ToString();
        }

        public override string GetJustification() {
            return "Cancellation: " + cancellationOf.ProofStep.GetStepNumber() + " " + subst;
        }
    }
}
