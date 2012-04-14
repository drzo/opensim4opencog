using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent.Impl;

    public class VacuumEnvPercept : DynamicPercept 
    {
        public static readonly string AttributeAgentLocation = "agentLocation";
        public static readonly string AttributeState = "state";

        public VacuumEnvPercept(string agentLocation, VacuumEnvironment.LocationState state) 
        {
            SetAttribute(AttributeAgentLocation, agentLocation);
            SetAttribute(AttributeState, state);
        }

        public string GetAgentLocation() 
        {
            return (string) GetAttribute(AttributeAgentLocation);
        }

        public VacuumEnvironment.LocationState GetLocationState() 
        {
            return (VacuumEnvironment.LocationState) GetAttribute(AttributeState);
        }

        public override string ToString() 
        {
            return String.Format("[{0}, {1}]", this.GetAgentLocation(), this.GetLocationState());
        }
    }

}
