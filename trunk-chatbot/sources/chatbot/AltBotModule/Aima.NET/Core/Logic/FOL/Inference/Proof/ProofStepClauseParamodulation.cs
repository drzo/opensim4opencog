using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepClauseParamodulation : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause paramodulated;
        private Clause topClause;
        private Clause equalityClause;
        private TermEquality assertion;

        public ProofStepClauseParamodulation(Clause paramodulated,
                Clause topClause, Clause equalityClause, TermEquality assertion) 
        {
            this.paramodulated = paramodulated;
            this.topClause = topClause;
            this.equalityClause = equalityClause;
            this.assertion = assertion;
            this.predecessors.Add(topClause.GetProofStep());
            this.predecessors.Add(equalityClause.GetProofStep());
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return paramodulated.ToString();
        }

        public override string GetJustification() 
        {
            return String.Format("Paramodulation: {0}, {1}, [{2}]", topClause.GetProofStep().GetStepNumber()
                    , equalityClause.GetProofStep().GetStepNumber(), assertion);

        }
    }
}
