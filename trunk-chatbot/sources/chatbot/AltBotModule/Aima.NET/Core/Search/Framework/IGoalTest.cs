using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 67. <para />
    /// The goal test, which determines whether a given state is a goal state.
    /// </summary>
    public interface IGoalTest
    {
        bool IsGoalState(object state);
    }
}
