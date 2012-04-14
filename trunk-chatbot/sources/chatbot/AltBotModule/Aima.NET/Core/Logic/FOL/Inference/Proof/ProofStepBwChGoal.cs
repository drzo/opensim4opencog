using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepBwChGoal : AbstractProofStep 
    {
        //
        private IList<IProofStep> predecessors = new List<IProofStep>();
        //
        private Clause toProve = null;
        private Literal currentGoal = null;
        private IDictionary<Variable, ITerm> bindings = new Dictionary<Variable, ITerm>();

        public ProofStepBwChGoal(Clause toProve, Literal currentGoal,
                IDictionary<Variable, ITerm> bindings) {
            this.toProve = toProve;
            this.currentGoal = currentGoal;
            foreach (var row in bindings)
            {
                this.Bindings.Add(row);
            }
        }

        public IDictionary<Variable, ITerm> Bindings
        {
            get
            {
                return this.bindings;
            }
        }

        public void SetPredecessor(IProofStep predecessor) 
        {
            predecessors.Clear();
            predecessors.Add(predecessor);
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            StringBuilder sb = new StringBuilder();
            IList<Literal> nLits = toProve.GetNegativeLiterals();
            for (int i = 0; i < toProve.GetNumberNegativeLiterals(); i++) {
                sb.Append(nLits[i].AtomicSentence);
                if (i != (toProve.GetNumberNegativeLiterals() - 1)) {
                    sb.Append(" AND ");
                }
            }
            if (toProve.GetNumberNegativeLiterals() > 0) {
                sb.Append(" => ");
            }
            sb.Append(toProve.GetPositiveLiterals()[0]);
            return sb.ToString();
        }

        public override string GetJustification() {
            return "Current Goal " + currentGoal.AtomicSentence + ", " + this.Bindings;
        }
        // END-IProofStep
        //
    }

}
