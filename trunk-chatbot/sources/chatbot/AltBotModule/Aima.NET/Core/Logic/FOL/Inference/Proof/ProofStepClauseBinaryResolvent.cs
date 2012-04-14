using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using System.Collections.ObjectModel;

    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofStepClauseBinaryResolvent : AbstractProofStep 
    {
        private IList<IProofStep> predecessors = new List<IProofStep>();
        private Clause resolvent;
        private Clause parent1, parent2;
        private IDictionary<Variable, ITerm> subst;
        private IDictionary<Variable, ITerm> renameSubst;

        public ProofStepClauseBinaryResolvent(Clause resolvent, Clause parent1,
                Clause parent2, IDictionary<Variable, ITerm> subst, IDictionary<Variable, ITerm> renameSubst) 
        {
            this.resolvent = resolvent;
            this.parent1 = parent1;
            this.parent2 = parent2;
            this.subst = subst;
            this.renameSubst = renameSubst;
            this.predecessors.Add(parent1.GetProofStep());
            this.predecessors.Add(parent2.GetProofStep());
        }

        public override IList<IProofStep> GetPredecessorSteps() 
        {
            return new ReadOnlyCollection<IProofStep>(predecessors);
        }

        public override string GetProof() 
        {
            return resolvent.ToString();
        }

        public override string GetJustification()
        {
            var lowStep = this.parent1.GetProofStep().GetStepNumber();
            var highStep = this.parent2.GetProofStep().GetStepNumber();

            if (lowStep > highStep)
            {
                lowStep = highStep;
                highStep = this.parent1.GetProofStep().GetStepNumber();
            }

            return String.Format("Resolution: {0},{1} {2}, {3}", lowStep, highStep, this.subst, this.renameSubst);
        }
    }
}
