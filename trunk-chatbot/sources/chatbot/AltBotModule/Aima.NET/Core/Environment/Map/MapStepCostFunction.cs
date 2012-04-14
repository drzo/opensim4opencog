using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Implementation of StepCostFunction interface that uses the distance between locations
    /// to calculate the cost in addition to a constant cost, so that it may be used
    /// in conjunction with a Uniform-cost search.
    /// </summary>
    public class MapStepCostFunction : IStepCostFunction {
        private IMap map = null;

        /// <summary>
        /// Used by Uniform-cost search to ensure every step is greater than or equal
        /// to some small positive constant
        /// </summary>
        private static double constantCost = 1.0;

        public MapStepCostFunction(IMap aMap) {
            this.map = aMap;
        }

        /// <summary>
        /// START-StepCostFunction
        /// </summary>
        /// <param name="fromCurrentState"></param>
        /// <param name="action"></param>
        /// <param name="toNextState"></param>
        /// <returns></returns>
        public double C(object fromCurrentState, IAction action, object toNextState)
        {
            var fromLoc = fromCurrentState.ToString();
            var toLoc = toNextState.ToString();

            var distance = this.map.GetDistance(fromLoc, toLoc);

            // TODO: figure out if null is a value that actually can be returned from previous method. The check for it was here originally.
            if (distance <= 0)
            {
                return constantCost;
            }

            return distance;
        }

        // END-StepCostFunction
        //
    }

}
