using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// This class extends heuristic functions in two ways: It maintains a goal and a
    /// map to estimate distance to goal for states in route planning problems, and
    /// it provides a method to adapt to different goals.
    /// </summary>
    public abstract class AdaptableHeuristicFunction : IHeuristicFunction 
    //TODO: this originally inherited java's Cloneable interface which wasn't implemented. Figure out if needed at all
    {
        /// <summary>
        /// The Current Goal.
        /// </summary>
        protected object Goal;
        
        /// <summary>
        /// The map to be used for distance to goal estimates.
        /// </summary>
        protected IMap Map;

        /// <summary>
        /// Modifies goal and map information and returns the modified heuristic
        /// function.
        /// </summary>
        /// <param name="goal"></param>
        /// <param name="map"></param>
        /// <returns></returns>
        public AdaptableHeuristicFunction AdaptToGoal(object goal, IMap map) {
            this.Goal = goal;
            this.Map = map;
            return this;
        }

        public abstract double H(object state);
    }

}
