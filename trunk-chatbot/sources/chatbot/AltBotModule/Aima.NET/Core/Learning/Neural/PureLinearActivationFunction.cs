using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public class PureLinearActivationFunction : IActivationFunction 
    {

        public double Activation(double parameter) {
            return parameter;
        }

        public double Deriv(double parameter) {

            return 1;
        }
    }
}
