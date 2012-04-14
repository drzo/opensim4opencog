using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepClauseDemodulation : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause demodulated = null;
        private Clause origClause = null;
        private TermEquality assertion = null;

        public ProofStepClauseDemodulation(Clause demodulated, Clause origClause,
                TermEquality assertion) {
            this.demodulated = demodulated;
            this.origClause = origClause;
            this.assertion = assertion;
            this.predecessors.Add(origClause.GetProofStep());
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return demodulated.ToString();
        }

        public override string GetJustification() {
            return "Demodulation: " + origClause.GetProofStep().GetStepNumber()
                    + ", [" + assertion + "]";
        }
    }
}
