using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 67. <para />
    /// A description of what each action does; the formal name for this is the
    /// transition model, specified by a function RESULT(s, a) that returns the state
    /// that results from doing action a in state s. We also use the term successor
    /// to refer to any state reachable from a given state by a single action.
    /// </summary>
    public interface IResultFunction
    {
        // TODO: think about getting rid of the object type below. Should be able to replace with interface
        /// <summary>
        /// Returns the state that results from doing action a in state s
        /// </summary>
        /// <param name="s">a particular state.</param>
        /// <param name="a">an action to be performed in state s.</param>
        /// <returns>the state that results from doing action a in state s.</returns>
        object Result(object s, IAction a);
    }
}
