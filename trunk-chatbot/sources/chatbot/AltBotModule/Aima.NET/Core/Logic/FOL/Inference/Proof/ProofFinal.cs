using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class ProofFinal : IProof 
    {
        private IDictionary<Variable, ITerm> answerBindings = new Dictionary<Variable, ITerm>();
        private IProofStep finalStep;
        private IList<IProofStep> proofSteps;

        public ProofFinal(IProofStep finalStep, IDictionary<Variable, ITerm> answerBindings) 
        {
            this.finalStep = finalStep;
            this.answerBindings = answerBindings;
        }

        public IList<IProofStep> GetSteps() 
        {
            // Only calculate if the proof steps are actually requested.
            if (null == proofSteps) 
            {
                this.CalcualteProofSteps();
            }
            return proofSteps;
        }

        public IDictionary<Variable, ITerm> GetAnswerBindings() 
        {
            return answerBindings;
        }

        public void ReplaceAnswerBindings(IDictionary<Variable, ITerm> updatedBindings) 
        {
            answerBindings = updatedBindings;
        }

        public override string ToString() 
        {
            return answerBindings.ToString();
        }

        private void CalcualteProofSteps() 
        {
            proofSteps = new List<IProofStep>();
            this.AddToProofSteps(finalStep);

            // Move all premises to the front of the
            // list of steps
            int to = 0;
            for (int i = 0; i < proofSteps.Count; i++) 
            {
                if (proofSteps[i] is ProofStepPremise)
                {
                    IProofStep m = proofSteps[i];
                    proofSteps.RemoveAt(i);
                    proofSteps[to] = m;
                    to++;
                }
            }

            // Move the Goals after the premises
            for (int i = 0; i < proofSteps.Count; i++) 
            {
                if (proofSteps[i] is ProofStepGoal) 
                {
                    IProofStep m = proofSteps[i];
                    proofSteps.RemoveAt(i);
                    proofSteps[to] = m;
                    to++;
                }
            }

            // Assign the step #s now that all the proof
            // steps have been unwound
            for (int i = 0; i < proofSteps.Count; i++) 
            {
                proofSteps[i].SetStepNumber(i + 1);
            }
        }

        private void AddToProofSteps(IProofStep step) 
        {
            if (!proofSteps.Contains(step)) 
            {
                proofSteps[0] = step;
            } 
            else 
            {
                proofSteps.Remove(step);
                proofSteps[0] = step;
            }
            IList<IProofStep> predecessors = step.GetPredecessorSteps();
            for (int i = predecessors.Count - 1; i >= 0; i--) 
            {
                this.AddToProofSteps(predecessors[i]);
            }
        }
    }
}
