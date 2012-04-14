using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Util.Datastructure;
    using Aima.Core.Util.Math;

    public class FeedForwardNeuralNetwork : IFunctionApproximator 
    {
        public static readonly String UpperLimitWeights = "upper_limit_weights";
        public static readonly String LowerLimitWeights = "lower_limit_weights";
        public static readonly String NumberOfOutputs = "number_of_outputs";
        public static readonly String NumberOfHiddenNeurons = "number_of_hidden_neurons";
        public static readonly String NumberOfInputs = "number_of_inputs";

        private readonly Layer hiddenLayer;
        private readonly Layer outputLayer;

        private INNTrainingScheme trainingScheme;

        /*
         * constructor to be used for non testing code.
         */
        public FeedForwardNeuralNetwork(NNConfig config) {

            int numberOfInputNeurons = config
                    .GetParameterAsInteger(NumberOfInputs);
            int numberOfHiddenNeurons = config
                    .GetParameterAsInteger(NumberOfHiddenNeurons);
            int numberOfOutputNeurons = config
                    .GetParameterAsInteger(NumberOfOutputs);

            double lowerLimitForWeights = config
                    .GetParameterAsDouble(LowerLimitWeights);
            double upperLimitForWeights = config
                    .GetParameterAsDouble(UpperLimitWeights);

            hiddenLayer = new Layer(numberOfHiddenNeurons, numberOfInputNeurons,
                    lowerLimitForWeights, upperLimitForWeights,
                    new LogSigActivationFunction());

            outputLayer = new Layer(numberOfOutputNeurons, numberOfHiddenNeurons,
                    lowerLimitForWeights, upperLimitForWeights,
                    new PureLinearActivationFunction());

        }

        /*
         * ONLY for testing to set up a network with known weights in future use to
         * deserialize networks after adding variables for pen weightupdate,
         * lastnput etc
         */
        public FeedForwardNeuralNetwork(Matrix hiddenLayerWeights,
                Vector hiddenLayerBias, Matrix outputLayerWeights,
                Vector outputLayerBias) {

            hiddenLayer = new Layer(hiddenLayerWeights, hiddenLayerBias,
                    new LogSigActivationFunction());
            outputLayer = new Layer(outputLayerWeights, outputLayerBias,
                    new PureLinearActivationFunction());

        }

        public void ProcessError(Vector error) {

            trainingScheme.ProcessError(this, error);

        }

        public Vector ProcessInput(Vector input) {
            return trainingScheme.ProcessInput(this, input);
        }

        public void TrainOn(NNDataSet innds, int numberofEpochs) 
        {
            for (int i = 0; i < numberofEpochs; i++)
            {
                innds.RefreshDataset();
                while (innds.HasMoreExamples())
                {
                    NNExample nne = innds.GetExampleAtRandom();
                    this.ProcessInput(nne.GetInput());
                    Vector error = this.GetOutputLayer().ErrorVectorFrom(nne.GetTarget());
                    this.ProcessError(error);
                }
            }
        }

        public Vector Predict(NNExample nne) {
            return this.ProcessInput(nne.GetInput());
        }

        public int[] TestOnDataSet(NNDataSet nnds)
        {
            int[] result = new int[] { 0, 0 };
            nnds.RefreshDataset();
            while (nnds.HasMoreExamples())
            {
                NNExample nne = nnds.GetExampleAtRandom();
                Vector prediction = this.Predict(nne);
                if (nne.IsCorrect(prediction))
                {
                    result[0] = result[0] + 1;
                }
                else
                {
                    result[1] = result[1] + 1;
                }
            }
            return result;
        }

        public void TestOn(DataSet ds) {
            // TODO Auto-generated method stub
        }

        public Matrix GetHiddenLayerWeights() {

            return hiddenLayer.GetWeightMatrix();
        }

        public Vector GetHiddenLayerBias() {

            return hiddenLayer.GetBiasVector();
        }

        public Matrix GetOutputLayerWeights() {

            return outputLayer.GetWeightMatrix();
        }

        public Vector GetOutputLayerBias() {

            return outputLayer.GetBiasVector();
        }

        public Layer GetHiddenLayer() {
            return hiddenLayer;
        }

        public Layer GetOutputLayer() {
            return outputLayer;
        }

        public void SetTrainingScheme(INNTrainingScheme tScheme) 
        {
            this.trainingScheme = tScheme;
            tScheme.SetNeuralNetwork(this);
        }
    }
}
