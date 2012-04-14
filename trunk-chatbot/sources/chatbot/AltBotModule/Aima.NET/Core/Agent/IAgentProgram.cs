using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): pg 35.<br />
    /// An agent's behavior is described by the 'agent function' that maps any given percept
    /// sequence to an action. Internally, the agent function for an artificial agent will be
    /// implemented by an agent program.
    /// </summary>
    public interface IAgentProgram
    {
        /// <summary>
        /// The Agent's program, which maps any given percept sequences to an action.
        /// </summary>
        /// <param name="percept">The current percept of a sequence perceived by the Agent.</param>
        /// <returns>the Action to be taken in response to the currently perceived percept.</returns>
        IAction Execute(IPercept percept);
    }
}
