using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepClauseClausifySentence : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause clausified = null;

        public ProofStepClauseClausifySentence(Clause clausified,
                ISentence origSentence) {
            this.clausified = clausified;
            this.predecessors.Add(new ProofStepPremise(origSentence));
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            return clausified.ToString();
        }

        public override string GetJustification() 
        {
            return "Clausified " + predecessors[0].GetStepNumber();
        }
        // END-ProofStep
        //
    }

}
