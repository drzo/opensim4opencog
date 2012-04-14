using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent
{
    /// <summary>
    /// Allows external applications/logic to view the interaction of Agent(s) with an Environment.
    /// </summary>
    public interface IEnvironmentView
    {
        /// <summary>
        /// A simple notification message from the Environment, from one of its
        /// objects.
        /// </summary>
        /// <param name="msg">the message received.</param>
        void Notify(string msg);
 
        /// <summary>
        /// Indicates an Agent has been added to the environment and what it
        /// perceives initially. 
        /// </summary>
        /// <param name="agent">the Agent just added to the Environment.</param>
        /// <param name="resultingState">the EnvironmentState that resulted from the Agent being added to the Environment.</param>
        void AgentAdded(IAgent agent, IEnvironmentState resultingState);

        /// <summary>
        /// Indicates the Environment has changed as a result of an Agent's action. 
        /// </summary>
        /// <param name="agent">the Agent that performed the Action.</param>
        /// <param name="action">the Action the Agent performed.</param>
        /// <param name="resultingState">the EnvironmentState that resulted from the Agent's Action on the Environment.</param>
        void AgentActed(IAgent agent, IAction action, IEnvironmentState resultingState);
    }
}