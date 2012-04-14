using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Map
{
    using Aima.Core.Agent;
    using Aima.Core.Util.Datastructure;

    public class MapEnvironmentState : IEnvironmentState 
    {
        private IDictionary<IAgent, Pair<string, double>> agentLocationAndTravelDistance = new Dictionary<IAgent, Pair<string, double>>();

        public string GetAgentLocation(IAgent a)
        {
            var locAndTDistance = this.agentLocationAndTravelDistance[a];
            return locAndTDistance == null ? null : locAndTDistance.GetFirst();
        }

        public double GetAgentTravelDistance(IAgent a) 
        {
            var locAndTDistance = this.agentLocationAndTravelDistance[a];
            return locAndTDistance == null ? 0 : locAndTDistance.GetSecond();
        }

        public void SetAgentLocationAndTravelDistance(IAgent a, string location, double travelDistance)
        {
            this.agentLocationAndTravelDistance[a] = new Pair<string, double>(location, travelDistance);
        }
    }
}
