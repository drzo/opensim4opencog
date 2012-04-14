using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Checks whether a given state equals an explicitly specified goal state.
    /// </summary>
    public class DefaultGoalTest : IGoalTest {
        private object goalState;

        public DefaultGoalTest(object goalState) {
            this.goalState = goalState;
        }

        public bool IsGoalState(object state) {
            return goalState.Equals(state);
        }
    }
}
