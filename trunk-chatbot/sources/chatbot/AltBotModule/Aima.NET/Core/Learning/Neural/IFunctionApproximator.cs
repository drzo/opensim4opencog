using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    using System.Collections;

    using Aima.Core.Util.Math;

    public interface IFunctionApproximator
    {
        /// <summary>
        /// accepts input pattern and processe it returning an output value
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
        Vector ProcessInput(Vector input);

        /// <summary>
        /// accept an error and change the parameters to accomodate it
        /// </summary>
        /// <param name="error"></param>
        void ProcessError(Vector error);
    }
}
