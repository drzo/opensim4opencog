using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent;

    public class VacuumEnvironmentState : IEnvironmentState 
    {
        private IDictionary<string, VacuumEnvironment.LocationState> state;
        private IDictionary<IAgent, string> agentLocations;

        public VacuumEnvironmentState() {
            state = new Dictionary<string, VacuumEnvironment.LocationState>();
            agentLocations = new Dictionary<IAgent, string>();
        }

        public VacuumEnvironmentState(VacuumEnvironment.LocationState locAState, VacuumEnvironment.LocationState locBState): this() 
        {
            state[VacuumEnvironment.LocationA] = locAState;
            state[VacuumEnvironment.LocationB] = locBState;
        }

        public String GetAgentLocation(IAgent a) 
        {
            return agentLocations[a];
        }

        public void SetAgentLocation(IAgent a, string location) 
        {
            agentLocations[a] = location;
        }

        public VacuumEnvironment.LocationState GetLocationState(string location) 
        {
            return state[location];
        }

        public void setLocationState(String location,
                VacuumEnvironment.LocationState s) {
            state[location] = s;
        }

        public override String ToString() {
            return state.ToString();
        }
    }

}
