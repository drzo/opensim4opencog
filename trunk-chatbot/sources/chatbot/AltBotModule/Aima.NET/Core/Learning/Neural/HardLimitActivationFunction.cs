using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public class HardLimitActivationFunction : IActivationFunction 
    {

        public double Activation(double parameter)
        {
            if (parameter < 0.0)
            {
                return 0.0;
            }
            return 1.0;
        }

        public double Deriv(double parameter) 
        {
            return 0.0;
        }
    }
}
