using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    /// <summary>
    /// A specialization of the GoalTest interface so that it is possible to check
    /// the solution once a Goal has been identified to determine if it is
    /// acceptable. This allows you to continue searching for alternative solutions
    /// without having to restart the search.
    /// 
    /// However, care needs to be taken when doing this as it does not always make
    /// sense to continue with a search once an initial goal is found, for example if
    /// using a heuristic targeted at a single goal.
    /// </summary>
    public interface ISolutionChecker : IGoalTest 
    {
        /// <summary>
        /// This method is only called if GoalTest.isGoalState() returns true.
        /// 
        /// @param actions
        ///            
        /// 
        /// @param goal
        ///            
        /// 
        /// @return 
        /// </summary>
        /// <param name="actions">the list of actions to get to the goal state.</param>
        /// <param name="goal">the goal the list of actions will reach.</param>
        /// <returns>true if the solution is acceptable, false otherwise, which 
        /// indicates the search should be continued.</returns>
        bool IsAcceptableSolution(IList<IAction> actions, object goal);
    }
}
