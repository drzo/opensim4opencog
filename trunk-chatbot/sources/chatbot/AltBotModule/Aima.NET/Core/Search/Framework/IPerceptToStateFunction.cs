using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    /// <summary>
    /// This interface is to define how to Map a Percept to a State representation
    /// for a problem solver within a specific environment. This arises in the
    /// description of the Online Search algorithms from Chapter 4.
    /// </summary>
    public interface IPerceptToStateFunction
    {
        /// <summary>
        /// Get the problem state associated with a Percept.
        /// </summary>
        /// <param name="p">the percept to be transformed to a problem state.</param>
        /// <returns>a problem state derived from the Percept p.</returns>
        object GetState(IPercept p);
    }
}
