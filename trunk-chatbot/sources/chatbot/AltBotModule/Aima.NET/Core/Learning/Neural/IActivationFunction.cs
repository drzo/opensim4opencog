using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public interface IActivationFunction
    {
        double Activation(double parameter);

        double Deriv(double parameter);
    }
}
