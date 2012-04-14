using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    using Aima.Core.Util.Datastructure;

    public class HiddenMarkovModel
    {

        public SensorModel SensorModel { get; private set; }

        public TransitionModel TransitionModel { get; private set; }

        private RandomVariable priorDistribution;

        public HiddenMarkovModel(RandomVariable priorDistribution,
                TransitionModel tm, SensorModel sm)
        {
            this.priorDistribution = priorDistribution;
            this.TransitionModel = tm;
            this.SensorModel = sm;
        }

        public RandomVariable Prior()
        {
            return priorDistribution;
        }

        public RandomVariable Predict(RandomVariable aBelief, string action)
        {
            RandomVariable newBelief = aBelief.Duplicate();

            Matrix beliefMatrix = aBelief.AsMatrix();
            Matrix transitionMatrix = this.TransitionModel.AsMatrix(action);
            Matrix predicted = transitionMatrix.Transpose().Times(beliefMatrix);
            newBelief.UpdateFrom(predicted);
            return newBelief;
        }

        public RandomVariable PerceptionUpdate(RandomVariable aBelief,
                string perception)
        {
            RandomVariable newBelief = aBelief.Duplicate();

            // one way - use matrices
            Matrix beliefMatrix = aBelief.AsMatrix();
            Matrix oMatrix = this.SensorModel.AsMatrix(perception);
            Matrix updated = oMatrix.Times(beliefMatrix);
            newBelief.UpdateFrom(updated);
            newBelief.Normalize();
            return newBelief;

            // alternate way of doing this. clearer in intent.
            // for (string state : aBelief.states()){
            // double probabilityOfPerception= sensorModel.get(state,perception);
            // newBelief.setProbabilityOf(state,probabilityOfPerception *
            // aBelief.getProbabilityOf(state));
            // }
        }

        public RandomVariable Forward(RandomVariable aBelief, string action,
                string perception)
        {
            return this.PerceptionUpdate(this.Predict(aBelief, action), perception);
        }

        public RandomVariable Forward(RandomVariable aBelief, string perception)
        {
            return this.Forward(aBelief, HmmConstants.DoNothing, perception);
        }

        public RandomVariable CalculateNextBackwardMessage(
                RandomVariable forwardBelief,
                RandomVariable presentBackwardMessage, string perception)
        {
            RandomVariable result = presentBackwardMessage.Duplicate();
            // System.out.println("fb :-calculating new backward message");
            // System.out.println("fb :-diagonal matrix from sens model = ");
            Matrix oMatrix = this.SensorModel.AsMatrix(perception);
            // System.out.println(oMatrix);
            Matrix transitionMatrix = this.TransitionModel.AsMatrix();// action
            // should
            // be
            // passed
            // in
            // here?
            // System.out.println("fb :-present backward message = "
            // +present_backward_message);
            Matrix backwardMatrix = transitionMatrix.Times(oMatrix
                    .Times(presentBackwardMessage.AsMatrix()));
            Matrix resultMatrix = backwardMatrix.ArrayTimes(forwardBelief
                    .AsMatrix());
            result.UpdateFrom(resultMatrix);
            result.Normalize();
            // System.out.println("fb :-normalized new backward message = "
            // +result);
            return result;
        }

        public List<RandomVariable> ForwardBackward(List<string> perceptions) 
        {
            RandomVariable[] forwardMessages = new RandomVariable[perceptions.Count + 1];
            RandomVariable backwardMessage = priorDistribution.CreateUnitBelief();
            RandomVariable[] smoothedBeliefs = new RandomVariable[perceptions.Count + 1];

            forwardMessages[0] = priorDistribution;
            smoothedBeliefs[0] = null;

            // populate forward messages
            for (int i = 0; i < perceptions.Count; i++) { // N.B i starts at 1,
                // not zero
                forwardMessages[i + 1] = this.Forward(forwardMessages[i], perceptions[i]);
            }
            for (int i = perceptions.Count; i > 0; i--) {
                RandomVariable smoothed = priorDistribution.Duplicate();
                smoothed.UpdateFrom(forwardMessages[i].AsMatrix().ArrayTimes(
                        backwardMessage.AsMatrix()));
                smoothed.Normalize();
                smoothedBeliefs[i] = smoothed;
                backwardMessage = this.CalculateNextBackwardMessage(
                        forwardMessages[i], backwardMessage, perceptions[i - 1]);
            }

            return smoothedBeliefs.ToList();
        }
    }
}
