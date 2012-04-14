using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    using Aima.Core.Util.Datastructure;

    public class TransitionModel
    {

        private Table<string, string, double> table;

        private IList<string> states;

        public TransitionModel(IList<string> states, IList<string> actions) {
            this.states = states;
            var stateActions = new List<string>();
            foreach (string state in states)
            {
                string state1 = state;
                stateActions.AddRange(actions.Select(action => state1 + action));
            }
                table = new Table<string, string, double>(stateActions, states);
        }

            // no actions possible thus the only "action" is to "wait" till the next
            // perception is observed

        public TransitionModel(IList<string> states) : this(states, new[] { HmmConstants.DoNothing }.ToList())
        {
            
        }

        public void SetTransitionProbability(string startState, string endState,
                double probability)
        {
            string startStatePlusAction = startState + HmmConstants.DoNothing;
            table.Set(startStatePlusAction, endState, probability);
        }

        public void SetTransitionProbability(string startState, string action,
                string endState, double probability)
        {
            string startStatePlusAction = startState + action;
            table.Set(startStatePlusAction, endState, probability);
        }

        public double Get(string oldStateAction, string newState)
        {
            return table.Get(oldStateAction, newState);
        }

        public Matrix AsMatrix(string action)
        {
            Matrix transitionMatrix = new Matrix(states.Count, states.Count);
            for (int i = 0; i < states.Count; i++)
            {
                string oldState = states[i];
                string oldStateAction = oldState + action;
                for (int j = 0; j < states.Count; j++)
                {
                    string newState = states[j];
                    double transitionProbability = this.Get(oldStateAction, newState);
                    transitionMatrix.Set(i, j, transitionProbability);
                }
            }
            return transitionMatrix;
        }

        public Matrix AsMatrix()
        {
            return this.AsMatrix(HmmConstants.DoNothing);
        }

        public Matrix UnitMatrix()
        {
            Matrix m = this.AsMatrix();
            return Matrix.Identity(m.GetRowDimension(), m.GetColumnDimension());
        }

        public string GetStateForProbability(string oldState, double probability)
        {
            return this.GetStateForGivenActionAndProbability(oldState,
                    HmmConstants.DoNothing, probability);
        }

        public string GetStateForProbability(string oldState, string action,
                double probability)
        {
            return this.GetStateForGivenActionAndProbability(oldState, action,
                    probability);
        }

        public string GetStateForGivenActionAndProbability(string oldState,
                string action, double probability)
        {
            string stateAction = oldState + action;

            double total = 0.0;
            foreach (string state in this.states)
            {
                total += this.table.Get(stateAction, state);
                if (total >= probability)
                {
                    return state;
                }
            }
            return null;
        }
    }
}
