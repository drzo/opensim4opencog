using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepChainReduction : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Chain reduction = null;
        private Chain nearParent, farParent = null;
        private IDictionary<Variable, ITerm> subst = null;

        public ProofStepChainReduction(Chain reduction, Chain nearParent,
                Chain farParent, IDictionary<Variable, ITerm> subst) {
            this.reduction = reduction;
            this.nearParent = nearParent;
            this.farParent = farParent;
            this.subst = subst;
            this.predecessors.Add(farParent.ProofStep);
            this.predecessors.Add(nearParent.ProofStep);
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            return reduction.ToString();
        }

        public override string GetJustification() {
            return "Reduction: " + nearParent.ProofStep.GetStepNumber() + ","
                    + farParent.ProofStep.GetStepNumber() + " " + subst;
        }
    }
}
