using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Util.Datastructure;
    using Aima.Core.Util.Math;

    public class BackPropLearning : INNTrainingScheme 
    {
        private readonly double learningRate;
        private readonly double momentum;

        private Layer hiddenLayer;
        private Layer outputLayer;
        private LayerSensitivity hiddenSensitivity;
        private LayerSensitivity outputSensitivity;

        public BackPropLearning(double learningRate, double momentum) {

            this.learningRate = learningRate;
            this.momentum = momentum;

        }

        public void SetNeuralNetwork(IFunctionApproximator fapp) {
            FeedForwardNeuralNetwork ffnn = (FeedForwardNeuralNetwork) fapp;
            this.hiddenLayer = ffnn.GetHiddenLayer();
            this.outputLayer = ffnn.GetOutputLayer();
            this.hiddenSensitivity = new LayerSensitivity(hiddenLayer);
            this.outputSensitivity = new LayerSensitivity(outputLayer);
        }

        public Vector ProcessInput(FeedForwardNeuralNetwork network, Vector input) {

            hiddenLayer.FeedForward(input);
            outputLayer.FeedForward(hiddenLayer.GetLastActivationValues());
            return outputLayer.GetLastActivationValues();
        }

        public void ProcessError(FeedForwardNeuralNetwork network, Vector error) {
            // TODO calculate total error somewhere
            // create Sensitivity Matrices
            outputSensitivity.SensitivityMatrixFromErrorMatrix(error);

            hiddenSensitivity
                    .SensitivityMatrixFromSucceedingLayer(outputSensitivity);

            // calculate weight Updates
            this.CalculateWeightUpdates(outputSensitivity, hiddenLayer
                    .GetLastActivationValues(), learningRate, momentum);
            this.CalculateWeightUpdates(hiddenSensitivity, hiddenLayer
                    .GetLastInputValues(), learningRate, momentum);

            // calculate Bias Updates
            this.CalculateBiasUpdates(outputSensitivity, learningRate, momentum);
            this.CalculateBiasUpdates(hiddenSensitivity, learningRate, momentum);

            // update weightsAndBiases
            outputLayer.UpdateWeights();
            outputLayer.UpdateBiases();

            hiddenLayer.UpdateWeights();
            hiddenLayer.UpdateBiases();

        }

        public Matrix CalculateWeightUpdates(LayerSensitivity layerSensitivity,
                Vector previousLayerActivationOrInput, double alpha, double momentum) {
            Layer layer = layerSensitivity.GetLayer();
            Matrix activationTranspose = previousLayerActivationOrInput.Transpose();
            Matrix momentumLessUpdate = layerSensitivity.GetSensitivityMatrix()
                    .Times(activationTranspose).Times(alpha).Times(-1.0);
            Matrix updateWithMomentum = layer.GetLastWeightUpdateMatrix().Times(
                    momentum).Plus(momentumLessUpdate.Times(1.0 - momentum));
            layer.AcceptNewWeightUpdate(updateWithMomentum.Copy());
            return updateWithMomentum;
        }

        public static Matrix CalculateWeightUpdates(
                LayerSensitivity layerSensitivity,
                Vector previousLayerActivationOrInput, double alpha) {
            Layer layer = layerSensitivity.GetLayer();
            Matrix activationTranspose = previousLayerActivationOrInput.Transpose();
            Matrix weightUpdateMatrix = layerSensitivity.GetSensitivityMatrix()
                    .Times(activationTranspose).Times(alpha).Times(-1.0);
            layer.AcceptNewWeightUpdate(weightUpdateMatrix.Copy());
            return weightUpdateMatrix;
        }

        public Vector CalculateBiasUpdates(LayerSensitivity layerSensitivity,
                double alpha, double mom) {
            Layer layer = layerSensitivity.GetLayer();
            Matrix biasUpdateMatrixWithoutMomentum = layerSensitivity
                    .GetSensitivityMatrix().Times(alpha).Times(-1.0);

            Matrix biasUpdateMatrixWithMomentum = layer.GetLastBiasUpdateVector()
                    .Times(mom).Plus(biasUpdateMatrixWithoutMomentum.Times(1.0 - mom));
            Vector result = new Vector(biasUpdateMatrixWithMomentum
                    .GetRowDimension());
            for (int i = 0; i < biasUpdateMatrixWithMomentum.GetRowDimension(); i++) {
                result.SetValue(i, biasUpdateMatrixWithMomentum.Get(i, 0));
            }
            layer.AcceptNewBiasUpdate(result.CopyVector());
            return result;
        }

        public static Vector CalculateBiasUpdates(
                LayerSensitivity layerSensitivity, double alpha) {
            Layer layer = layerSensitivity.GetLayer();
            Matrix biasUpdateMatrix = layerSensitivity.GetSensitivityMatrix()
                    .Times(alpha).Times(-1.0);

            Vector result = new Vector(biasUpdateMatrix.GetRowDimension());
            for (int i = 0; i < biasUpdateMatrix.GetRowDimension(); i++) {
                result.SetValue(i, biasUpdateMatrix.Get(i, 0));
            }
            layer.AcceptNewBiasUpdate(result.CopyVector());
            return result;
        }
    }

}
