using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    /// <summary>
    /// Returns one for every action.
    /// </summary>
    public class DefaultStepCostFunction : IStepCostFunction {

        public double C(object stateFrom, IAction action, object stateTo) {
            return 1;
        }
    }
}
