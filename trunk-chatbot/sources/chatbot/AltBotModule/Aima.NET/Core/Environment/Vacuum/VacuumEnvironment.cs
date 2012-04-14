using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    public class VacuumEnvironment : AbstractEnvironment 
    {
        // Allowable Actions within the Vaccum Environment
        public static readonly IAction ActionMoveLeft = new DynamicAction("Left");
        public static readonly IAction ActionMoveRight = new DynamicAction("Right");
        public static readonly IAction ActionSuck = new DynamicAction("Suck");
        public static readonly string LocationA = "A";
        public static readonly string LocationB = "B";

        public enum LocationState {
            Clean, Dirty
        };

        //
        protected VacuumEnvironmentState envState = null;

        private bool isDone = false;

        public VacuumEnvironment() 
        {
            var r = new Random();
            envState = new VacuumEnvironmentState(
                    0 == r.Next(2) ? LocationState.Clean : LocationState.Dirty,
                    0 == r.Next(2) ? LocationState.Clean : LocationState.Dirty);
        }

        public VacuumEnvironment(LocationState locAState, LocationState locBState) 
        {
            envState = new VacuumEnvironmentState(locAState, locBState);
        }

        public override bool IsDone()
        {
            return base.IsDone() || isDone;
        }

        public override IEnvironmentState GetCurrentState() 
        {
            return envState;
        }

        public override IEnvironmentState ExecuteAction(IAgent a, IAction agentAction) 
        {
            if (ActionMoveRight == agentAction) 
            {
                envState.SetAgentLocation(a, LocationB);
                UpdatePerformanceMeasure(a, -1);
            } 
            else if (ActionMoveLeft == agentAction) 
            {
                envState.SetAgentLocation(a, LocationA);
                UpdatePerformanceMeasure(a, -1);
            } 
            else if (ActionSuck == agentAction) 
            {
                if (LocationState.Dirty == envState.GetLocationState(envState
                        .GetAgentLocation(a))) 
                {
                    envState.setLocationState(envState.GetAgentLocation(a),
                            LocationState.Clean);
                    UpdatePerformanceMeasure(a, 10);
                }
            } 
            else if (agentAction.IsNoOp()) 
            {
                // In the Vacuum Environment we consider things done if
                // the agent generates a NoOp.
                isDone = true;
            }

            return envState;
        }

        public override IPercept GetPerceptSeenBy(IAgent anAgent) 
        {
            var agentLocation = envState.GetAgentLocation(anAgent);
            return new VacuumEnvPercept(agentLocation, envState
                    .GetLocationState(agentLocation));
        }

        public override void AddAgent(IAgent a) 
        {
            var idx = new Random().Next(2);
            envState.SetAgentLocation(a, idx == 0 ? LocationA : LocationB);
            base.AddAgent(a);
        }

        public void AddAgent(IAgent a, string location) 
        {
            // Ensure the agent state information is tracked before
            // adding to super, as super will notify the registered
            // EnvironmentViews that is was added.
            envState.SetAgentLocation(a, location);
            base.AddAgent(a);
        }

        public LocationState GetLocationState(String location) 
        {
            return envState.GetLocationState(location);
        }

        public String GetAgentLocation(IAgent a) {
            return envState.GetAgentLocation(a);
        }
    }
}
