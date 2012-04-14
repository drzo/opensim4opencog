// --------------------------------------------------------------------------------------------------------------------
// <copyright file="IEnvironment.cs" company="">
//   
// </copyright>
// <summary>
//   Defines the IEnvironment type.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace Aima.Core.Agent
{
    using System.Collections.Generic;

    /// <summary>
    /// An abstract description of possible discrete Environments in which Agent(s) 
    /// can perceive and act.
    /// </summary>
    public interface IEnvironment
    {    
        /// <summary>
        /// Move the Environment one time step forward. 
        /// </summary>
        void Step();

        /// <summary>
        /// Move the Environment n time steps forward.
        /// </summary>
        /// <param name="n">the number of time steps to move the Environment forward.</param>
        void Step(int n);

        /// <summary>
        /// Step through time steps until the Environment has no more tasks. 
        /// </summary>
        void StepUntilDone();

        /// <summary>
        /// Returns if the Environment is finished with its current task(s).
        /// </summary>
        /// <returns>if the Environment is finished with its current task(s).</returns>
        bool IsDone();

        /// <summary>
        /// Retrieve the performance measure associated with an Agent. 
        /// </summary>
        /// <param name="forAgent">the Agent for which a performance measure is to be retrieved.</param>
        /// <returns>the performance measure associated with the Agent.</returns>
        double GetPerformanceMeasure(IAgent forAgent);

        /// <summary>
        /// Add a view on the Environment.
        /// </summary>
        /// <param name="ev">the EnvironmentView to be added.</param>
        void AddEnvironmentView(IEnvironmentView ev);

        /// <summary>
        /// Remove a view on the Environment. 
        /// </summary>
        /// <param name="ev">the EnvironmentView to be removed.</param>
        void RemoveEnvironmentView(IEnvironmentView ev);

        /// <summary>
        /// Notify all registered EnvironmentViews of a message. 
        /// </summary>
        /// <param name="msg">the message to notify the registered EnvironmentViews with.</param>
        void NotifyViews(string msg);

        IList<IAgent> GetAgents();

        void AddAgent(IAgent a);

        void RemoveAgent(IAgent a);

        IList<IEnvironmentObject> GetEnvironmentObjects();

        void AddEnvironmentObject(IEnvironmentObject eo);

        void RemoveEnvironmentObject(IEnvironmentObject eo);
    }
}