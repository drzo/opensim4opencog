using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    using Aima.Core.Util.Datastructure;

    public class FixedLagSmoothing
    {

        // This implementation is almost certainly wrong (see comments below).
        // This is faithful to the algorithm in the book but I think there are
        // some errors in the algorithmas published. Need to clarify with Dr N.

        private HiddenMarkovModel hmm;

        private int timelag;

        private List<String> evidenceFromSmoothedStepToPresent;

        private int time;

        private RandomVariable forwardMessage;

        private Matrix B;

        public FixedLagSmoothing(HiddenMarkovModel hmm, int timelag)
        {
            this.hmm = hmm;
            this.timelag = timelag;
            this.evidenceFromSmoothedStepToPresent = new List<String>();
            this.time = 1;
            this.forwardMessage = hmm.Prior();
            this.B = hmm.TransitionModel.UnitMatrix();
        }

        public RandomVariable Smooth(String perception)
        {

            evidenceFromSmoothedStepToPresent.Add(perception);
            Matrix oT = hmm.SensorModel.AsMatrix(perception);
            Matrix transitionMatrix = hmm.TransitionModel.AsMatrix();
            if (time > timelag)
            {

                forwardMessage = hmm.Forward(forwardMessage, perception); // This
                // seems
                // WRONG
                // I think this should be
                // forwardMessage = hmm.forward(forwardMessage,
                // evidenceFromSmoothedStepToPresent.get(0));
                // this the perception at t-d. the book's algorithm
                // uses the latest perception.
                evidenceFromSmoothedStepToPresent.RemoveAt(0);
                Matrix oTMinusD = hmm.SensorModel.AsMatrix(
                        evidenceFromSmoothedStepToPresent[0]);

                B = oTMinusD.Inverse().Times(
                        transitionMatrix.Inverse().Times(
                                B.Times(transitionMatrix.Times(oT))));

            }
            else
            {

                B = B.Times(transitionMatrix.Times(oT));

            }
            time += 1;
            if (time > timelag)
            {

                Matrix one = hmm.Prior().CreateUnitBelief().AsMatrix();
                Matrix forwardMatrix = forwardMessage.AsMatrix();
                RandomVariable result = hmm.Prior().Duplicate();
                Matrix backwardMessage = (B.Times(one));

                result.UpdateFrom(forwardMatrix.ArrayTimes(backwardMessage));

                result.Normalize();
                return result;
            }
            else
            {
                return null;
            }
        }
    }
}
