namespace Aima.Core.Search.Framework
{
    using System;
    using System.Collections.Generic;
    using Iesi.Collections.Generic;

    using Aima.Core.Agent;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 67. <para />
    /// Given a particular state s, ACTIONS(s) returns the set of actions that can be
    /// executed in s. We say that each of these actions is <b>applicable</b> in s.
    /// </summary>
    public interface IActionsFunction
    {
        /// <summary>
        /// Given a particular state s, returns the set of actions that can be
        /// executed in s.
        /// </summary>
        /// <param name="s">a particular state.</param>
        /// <returns>the set of actions that can be executed in s.</returns>
        ISet<IAction> Actions(object s); 
    }

}
