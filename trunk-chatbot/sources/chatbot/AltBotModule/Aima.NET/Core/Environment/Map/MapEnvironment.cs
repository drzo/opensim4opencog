using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    /// <summary>
    /// Represents the environment a MapAgent can navigate.
    /// </summary>
    public class MapEnvironment : AbstractEnvironment {

        private IMap aMap;
        private MapEnvironmentState state = new MapEnvironmentState();

        public MapEnvironment(IMap aMap) {
            this.aMap = aMap;
        }

        public void AddAgent(IAgent a, string startLocation) 
        {
            // Ensure the agent state information is tracked before
            // adding to super, as super will notify the registered
            // EnvironmentViews that is was added.
            state.SetAgentLocationAndTravelDistance(a, startLocation, 0.0);
            base.AddAgent(a);
        }

        public string GetAgentLocation(IAgent a) {
            return state.GetAgentLocation(a);
        }

        public double GetAgentTravelDistance(IAgent a) {
            return state.GetAgentTravelDistance(a);
        }

        
        public override IEnvironmentState GetCurrentState() 
        {
            return state;
        }

        public override IEnvironmentState ExecuteAction(IAgent agent, IAction a) 
        {

            if (!a.IsNoOp()) 
            {
                var act = (MoveToAction) a;

                var currLoc = this.GetAgentLocation(agent);
                var distance = aMap.GetDistance(currLoc, act.GetToLocation());
                var currTd = this.GetAgentTravelDistance(agent);
                state.SetAgentLocationAndTravelDistance(agent, act.GetToLocation(), currTd + distance);
            }

            return state;
        }

        public override IPercept GetPerceptSeenBy(IAgent anAgent) 
        {
            return new DynamicPercept(DynAttributeNames.PerceptIn, this.GetAgentLocation(anAgent));
        }

        public IMap GetMap() 
        {
            return aMap;
        }
    }
}
