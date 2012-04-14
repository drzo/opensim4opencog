using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Agent.Impl.AProg;
    using Aima.Core.Agent.Impl.AProg.SimpleRule;

    public class ModelBasedReflexVacuumAgent : AbstractAgent 
    {
        private const String AttributeCurrentLocation = "currentLocation";
        private const String AttributeCurrentState = "currentState";
        private const String AttributeStateLocationA = "stateLocationA";
        private const String AttributeStateLocationB = "stateLocationB";

        public ModelBasedReflexVacuumAgent(): base(new ModelBasedReflexVacuumAgentProgram())
        {
        }

        private static ISet<Rule> GetRuleSet() {
            // Note: Using a LinkedHashSet so that the iteration order (i.e. implied
            // precedence) of rules can be guaranteed.
            ISet<Rule> rules = new HashedSet<Rule>();

            rules.Add(new Rule(new ANDCondition(new EQUALCondition(
                    AttributeStateLocationA,
                    VacuumEnvironment.LocationState.Clean), new EQUALCondition(
                    AttributeStateLocationB,
                    VacuumEnvironment.LocationState.Clean)), NoOpAction.NoOp));
            rules.Add(new Rule(new EQUALCondition(AttributeCurrentState,
                    VacuumEnvironment.LocationState.Dirty),
                    VacuumEnvironment.ActionSuck));
            rules.Add(new Rule(new EQUALCondition(AttributeCurrentLocation,
                    VacuumEnvironment.LocationA),
                    VacuumEnvironment.ActionMoveRight));
            rules.Add(new Rule(new EQUALCondition(AttributeCurrentLocation,
                    VacuumEnvironment.LocationB),
                    VacuumEnvironment.ActionMoveLeft));

            return rules;
        }
    private class ModelBasedReflexVacuumAgentProgram:ModelBasedReflexAgentProgram
    {
        protected override void Init() {
            SetState(new DynamicState());
            SetRules(GetRuleSet());
        }

        protected override DynamicState UpdateState(DynamicState envState,
                IAction anAction, IPercept percept, IModel model) {

            VacuumEnvPercept vep = (VacuumEnvPercept) percept;

            envState.SetAttribute(AttributeCurrentLocation, vep
                    .GetAgentLocation());
            envState.SetAttribute(AttributeCurrentState, vep
                    .GetLocationState());
            // Keep track of the state of the different locations
            if (VacuumEnvironment.LocationA == vep.GetAgentLocation()) {
                envState.SetAttribute(AttributeStateLocationA, vep
                        .GetLocationState());
            } else {
                envState.SetAttribute(AttributeStateLocationB, vep
                        .GetLocationState());
            }
            return envState;
        }
    }
    }
}
