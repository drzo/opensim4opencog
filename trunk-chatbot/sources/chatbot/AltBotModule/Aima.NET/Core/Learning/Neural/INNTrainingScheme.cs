using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using System.Collections;

    using Aima.Core.Util.Math;

    public interface INNTrainingScheme
    {
        Vector ProcessInput(FeedForwardNeuralNetwork network, Vector input);

        void ProcessError(FeedForwardNeuralNetwork network, Vector error);

        void SetNeuralNetwork(IFunctionApproximator ffnn);
    }
}
