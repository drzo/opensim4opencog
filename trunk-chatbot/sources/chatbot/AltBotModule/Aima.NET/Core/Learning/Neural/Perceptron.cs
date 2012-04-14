using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Util.Datastructure;
    using Aima.Core.Util.Math;

    public class Perceptron : IFunctionApproximator 
    {

        private readonly Layer layer;
        private Vector lastInput;

        public Perceptron(int numberOfNeurons, int numberOfInputs) {

            this.layer = new Layer(numberOfNeurons, numberOfInputs, 2.0, -2.0,
                    new HardLimitActivationFunction());

        }

        public Vector ProcessInput(Vector input) {
            lastInput = input;
            return layer.FeedForward(input);
        }

        public void ProcessError(Vector error) {
            Matrix weightUpdate = error.Times(lastInput.Transpose());
            layer.AcceptNewWeightUpdate(weightUpdate);

            Vector biasUpdate = layer.GetBiasVector().Plus(error);
            layer.AcceptNewBiasUpdate(biasUpdate);

        }

        public void TrainOn(NNDataSet innds, int numberofEpochs) {
            for (int i = 0; i < numberofEpochs; i++) {
                innds.RefreshDataset();
                while (innds.HasMoreExamples()) {
                    NNExample nne = innds.GetExampleAtRandom();
                    this.ProcessInput(nne.GetInput());
                    Vector error = layer.ErrorVectorFrom(nne.GetTarget());
                    this.ProcessError(error);
                }
            }
        }

        public Vector Predict(NNExample nne) {
            return this.ProcessInput(nne.GetInput());
        }

        public int[] TestOnDataSet(NNDataSet nnds) {
            int[] result = new int[] { 0, 0 };
            nnds.RefreshDataset();
            while (nnds.HasMoreExamples()) {
                NNExample nne = nnds.GetExampleAtRandom();
                Vector prediction = this.Predict(nne);
                if (nne.IsCorrect(prediction)) {
                    result[0] = result[0] + 1;
                } else {
                    result[1] = result[1] + 1;
                }
            }
            return result;
        }
    }

}
