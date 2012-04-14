using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepFoChAssertFact : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause implication = null;
        private Literal fact = null;
        private IDictionary<Variable, ITerm> bindings = null;

        public ProofStepFoChAssertFact(Clause implication, Literal fact,
                IDictionary<Variable, ITerm> bindings, IProofStep predecessor) {
            this.implication = implication;
            this.fact = fact;
            this.bindings = bindings;
            if (null != predecessor) {
                predecessors.Add(predecessor);
            }
        }

        public override IList<IProofStep> GetPredecessorSteps() {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() {
            var sb = new StringBuilder();
            IList<Literal> nLits = implication.GetNegativeLiterals();
            for (int i = 0; i < implication.GetNumberNegativeLiterals(); i++) {
                sb.Append(nLits[i].AtomicSentence);
                if (i != (implication.GetNumberNegativeLiterals() - 1)) {
                    sb.Append(" AND ");
                }
            }
            sb.Append(" => ");
            sb.Append(implication.GetPositiveLiterals()[0]);
            return sb.ToString();
        }

        public override string GetJustification() {
            return "Assert fact " + this.fact + ", " + bindings;
        }
    }

}
