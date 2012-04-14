using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using Aima.Core.Util.Datastructure;
    using Aima.Core.Util.Math;

    public class Layer 
    {
        // vectors are represented by n * 1 matrices;
        private readonly Matrix weightMatrix;

        Vector biasVector, lastBiasUpdateVector;

        private readonly IActivationFunction activationFunction;

        private Vector lastActivationValues, lastInducedField;

        private Matrix lastWeightUpdateMatrix;

        private Matrix penultimateWeightUpdateMatrix;

        private Vector penultimateBiasUpdateVector;

        private Vector lastInput;

        public Layer(Matrix weightMatrix, Vector biasVector, IActivationFunction af) {

            activationFunction = af;
            this.weightMatrix = weightMatrix;
            lastWeightUpdateMatrix = new Matrix(weightMatrix.GetRowDimension(),
                    weightMatrix.GetColumnDimension());
            penultimateWeightUpdateMatrix = new Matrix(weightMatrix
                    .GetRowDimension(), weightMatrix.GetColumnDimension());

            this.biasVector = biasVector;
            lastBiasUpdateVector = new Vector(biasVector.GetRowDimension());
            penultimateBiasUpdateVector = new Vector(biasVector.GetRowDimension());
        }

        public Layer(int numberOfNeurons, int numberOfInputs,
                double lowerLimitForWeights, double upperLimitForWeights,
                IActivationFunction af) {

            activationFunction = af;
            this.weightMatrix = new Matrix(numberOfNeurons, numberOfInputs);
            lastWeightUpdateMatrix = new Matrix(weightMatrix.GetRowDimension(),
                    weightMatrix.GetColumnDimension());
            penultimateWeightUpdateMatrix = new Matrix(weightMatrix
                    .GetRowDimension(), weightMatrix.GetColumnDimension());

            this.biasVector = new Vector(numberOfNeurons);
            lastBiasUpdateVector = new Vector(biasVector.GetRowDimension());
            penultimateBiasUpdateVector = new Vector(biasVector.GetRowDimension());

            InitializeMatrix(weightMatrix, lowerLimitForWeights,
                    upperLimitForWeights);
            InitializeVector(biasVector, lowerLimitForWeights, upperLimitForWeights);
        }

        public Vector FeedForward(Vector inputVector) {
            lastInput = inputVector;
            Matrix inducedField = weightMatrix.Times(inputVector).Plus(biasVector);

            Vector inducedFieldVector = new Vector(this.NumberOfNeurons());
            for (int i = 0; i < this.NumberOfNeurons(); i++) {
                inducedFieldVector.SetValue(i, inducedField.Get(i, 0));
            }

            lastInducedField = inducedFieldVector.CopyVector();
            Vector resultVector = new Vector(this.NumberOfNeurons());
            for (int i = 0; i < this.NumberOfNeurons(); i++) {
                resultVector.SetValue(i, activationFunction
                        .Activation(inducedFieldVector.GetValue(i)));
            }
            // set the result as the last activation value
            lastActivationValues = resultVector.CopyVector();
            return resultVector;
        }

        public Matrix GetWeightMatrix() {
            return weightMatrix;
        }

        public Vector GetBiasVector() {
            return biasVector;
        }

        public int NumberOfNeurons() {
            return weightMatrix.GetRowDimension();
        }

        public int NumberOfInputs() {
            return weightMatrix.GetColumnDimension();
        }

        public Vector GetLastActivationValues() {
            return lastActivationValues;
        }

        public Vector GetLastInducedField() {
            return lastInducedField;
        }

        public Matrix GetLastWeightUpdateMatrix() {
            return lastWeightUpdateMatrix;
        }

        public void SetLastWeightUpdateMatrix(Matrix m) {
            lastWeightUpdateMatrix = m;
        }

        public Matrix GetPenultimateWeightUpdateMatrix() {
            return penultimateWeightUpdateMatrix;
        }

        public void SetPenultimateWeightUpdateMatrix(Matrix m) {
            penultimateWeightUpdateMatrix = m;
        }

        public Vector GetLastBiasUpdateVector() {
            return lastBiasUpdateVector;
        }

        public void SetLastBiasUpdateVector(Vector v) {
            lastBiasUpdateVector = v;
        }

        public Vector GetPenultimateBiasUpdateVector() {
            return penultimateBiasUpdateVector;
        }

        public void SetPenultimateBiasUpdateVector(Vector v) {
            penultimateBiasUpdateVector = v;
        }

        public void UpdateWeights() {
            weightMatrix.PlusEquals(lastWeightUpdateMatrix);
        }

        public void UpdateBiases() {
            Matrix biasMatrix = biasVector.PlusEquals(lastBiasUpdateVector);
            Vector result = new Vector(biasMatrix.GetRowDimension());
            for (int i = 0; i < biasMatrix.GetRowDimension(); i++) {
                result.SetValue(i, biasMatrix.Get(i, 0));
            }
            biasVector = result;
        }

        public Vector GetLastInputValues() {

            return lastInput;

        }

        public IActivationFunction GetActivationFunction() {

            return activationFunction;
        }

        public void AcceptNewWeightUpdate(Matrix weightUpdate) {
            /*
             * penultimate weightupdates maintained only to implement VLBP later
             */
            this.SetPenultimateWeightUpdateMatrix(this.GetLastWeightUpdateMatrix());
            this.SetLastWeightUpdateMatrix(weightUpdate);
        }

        public void AcceptNewBiasUpdate(Vector biasUpdate) {
            this.SetPenultimateBiasUpdateVector(this.GetLastBiasUpdateVector());
            this.SetLastBiasUpdateVector(biasUpdate);
        }

        public Vector ErrorVectorFrom(Vector target) {
            return target.Minus(this.GetLastActivationValues());

        }

        private static void InitializeMatrix(Matrix aMatrix, double lowerLimit,
                double upperLimit) {
            for (int i = 0; i < aMatrix.GetRowDimension(); i++) {
                for (int j = 0; j < aMatrix.GetColumnDimension(); j++) {
                    double random = Util.Util.GenerateRandomDoubleBetween(lowerLimit,
                            upperLimit);
                    aMatrix.Set(i, j, random);
                }
            }

        }

        private static void InitializeVector(Vector aVector, double lowerLimit,
                double upperLimit) {
            for (int i = 0; i < aVector.Size(); i++) {

                double random = Util.Util.GenerateRandomDoubleBetween(lowerLimit,
                        upperLimit);
                aVector.SetValue(i, random);
            }
        }
    }
}
