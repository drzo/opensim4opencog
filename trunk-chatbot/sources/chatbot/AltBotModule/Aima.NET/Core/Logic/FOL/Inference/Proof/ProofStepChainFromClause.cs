using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;

    public class ProofStepChainFromClause : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Chain chain = null;
        private Clause fromClause = null;

        public ProofStepChainFromClause(Chain chain, Clause fromClause) {
            this.chain = chain;
            this.fromClause = fromClause;
            this.predecessors.Add(fromClause.GetProofStep());
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            return chain.ToString();
        }

        public override string GetJustification() {
            return "Chain from Clause: "
                    + fromClause.GetProofStep().GetStepNumber();
        }
    }
}
