// --------------------------------------------------------------------------------------------------------------------
// <copyright file="IAgent.cs" company="">
//   
// </copyright>
// <summary>
//   Defines the Agent type.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

using System;

namespace Aima.Core.Agent
{

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.1, page 35.<br />
    /// Figure 2.1 Agents interact with environments through sensors and actuators.
    /// </summary>
    public interface IAgent : IEnvironmentObject 
    {    
        /// <summary>
        /// Call the Agent's program, which maps any given percept sequences to an
        /// action.
        /// </summary>
        /// <param name="percept">The current percept of a sequence perceived by the Agent.</param>
        /// <returns>the Action to be taken in response to the currently perceived percept</returns>
        IAction Execute(IPercept percept);

        /// <summary>
        /// Gets or sets life-cycle indicator as to the liveness of an Agent.
        /// </summary>
        bool Alive { get; set; }

    }
}
