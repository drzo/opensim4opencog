// --------------------------------------------------------------------------------------------------------------------
// <copyright file="AbstractEnvironment.cs" company="">
//   
// </copyright>
// <summary>
//   Defines the AbstractEnvironment type.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace Aima.Core.Agent.Impl
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Iesi.Collections.Generic;

    /// <summary>
    /// </summary>
    public abstract class AbstractEnvironment : IEnvironment, INotifyEnvironmentViews
    {
        // Note: Use LinkedHashSet's in order to ensure order is respected as
        // provide
        // access to these elements via List interface.
        protected ISet<IEnvironmentObject> envObjects = new HashedSet<IEnvironmentObject>();

        protected ISet<IAgent> agents = new HashedSet<IAgent>();

        protected ISet<IEnvironmentView> views = new HashedSet<IEnvironmentView>();

        protected IDictionary<IAgent, Double> performanceMeasures = new Dictionary<IAgent, Double>();

        // 
        // Methods to be implemented by subclasses.
        public abstract IEnvironmentState GetCurrentState();

        public abstract IEnvironmentState ExecuteAction(IAgent agent, IAction action);

        public abstract IPercept GetPerceptSeenBy(IAgent anAgent);

        public IList<IAgent> GetAgents()
        {
            // Return as a List but also ensures the caller cannot modify
            return new List<IAgent>(agents);
        }

        public virtual void AddAgent(IAgent a)
        {
            envObjects.Add(a);
            agents.Add(a);
            AddEnvironmentObject(a);
        }

        public void RemoveAgent(IAgent a) 
        {
            envObjects.Remove(a);
            agents.Remove(a);
        }

        public IList<IEnvironmentObject> GetEnvironmentObjects() 
        {
            // Return as a List but also ensures the caller cannot modify
            return new List<IEnvironmentObject>(envObjects);
        }

        public void AddEnvironmentObject(IEnvironmentObject eo) 
        {
            envObjects.Add(eo);
        }

        public void RemoveEnvironmentObject(IEnvironmentObject eo) {
            envObjects.Remove(eo);
        }

        public void Step()
        {
            if (IsDone())
            {
                return;
            }
            foreach (IAgent agent in this.agents.Where(agent => agent.Alive)) 
            {
                var anAction = agent.Execute(this.GetPerceptSeenBy(agent));

                var es = this.ExecuteAction(agent, anAction);

                UpdateEnvironmentViewsAgentActed(agent, anAction, es);
            }
        }

        public void Step(int n)
        {
            for (var i = 0; i < n; i++)
            {
                Step();
            }
        }

        public void StepUntilDone()
        {
            while (!IsDone()) 
            {
                Step();
            }
        }

        public virtual bool IsDone() 
        {
            return !agents.Any(agent => agent.Alive);
        }

        public double GetPerformanceMeasure(IAgent forAgent)
        {
            if (!performanceMeasures.ContainsKey(forAgent))
            {
                performanceMeasures[forAgent] = 0;
            }

            return performanceMeasures[forAgent];
        }

        public void AddEnvironmentView(IEnvironmentView ev) 
        {
            views.Add(ev);
        }

        public void RemoveEnvironmentView(IEnvironmentView ev) 
        {
            views.Remove(ev);
        }

        public void NotifyViews(string msg) {
            foreach (var ev in views) 
            {
                ev.Notify(msg);
            }
        }

        protected void UpdatePerformanceMeasure(IAgent forAgent, double addTo)
        {
            performanceMeasures[forAgent] = GetPerformanceMeasure(forAgent) + addTo;
        }

        protected void UpdateEnvironmentViewsAgentAdded(IAgent agent)
        {
            foreach (var view in views) 
            {
                view.AgentAdded(agent, GetCurrentState());
            }
        }

        protected void UpdateEnvironmentViewsAgentActed(IAgent agent, IAction action,
                IEnvironmentState state)
        {
            foreach (var view in views)
            {
                view.AgentActed(agent, action, state);
            }
        }
    }
}
