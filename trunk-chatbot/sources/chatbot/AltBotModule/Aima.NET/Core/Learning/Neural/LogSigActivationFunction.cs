using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public class LogSigActivationFunction : IActivationFunction 
    {

        public double Activation(double parameter) {

            return 1.0 / (1.0 + Math.Pow(Math.E, (-1.0 * parameter)));
        }

        public double Deriv(double parameter) {
            // parameter = induced field
            // e == activation
            double e = 1.0 / (1.0 + Math.Pow(Math.E, (-1.0 * parameter)));
            return e * (1.0 - e);
        }
    }
}
