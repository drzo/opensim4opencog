using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent.Impl;
    using Aima.Core.Agent.Impl.AProg;
    using Aima.Core.Agent.Impl.AProg.SimpleRule;

    public class SimpleReflexVacuumAgent : AbstractAgent 
    {

        public SimpleReflexVacuumAgent()
            : base(new SimpleReflexAgentProgram(GetRuleSet()))
        {
        }

        //
        // PRIVATE METHODS
        //
        private static ISet<Rule> GetRuleSet() 
        {
            // Note: Using a LinkedHashSet so that the iteration order (i.e. implied
            // precedence) of rules can be guaranteed.
            ISet<Rule> rules = new HashedSet<Rule>();

            // Rules based on REFLEX-VACUUM-AGENT:
            // Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.8,
            // page 48.

            rules.Add(new Rule(new EQUALCondition(VacuumEnvPercept.AttributeState,
                    VacuumEnvironment.LocationState.Dirty),
                    VacuumEnvironment.ActionSuck));
            rules.Add(new Rule(new EQUALCondition(
                    VacuumEnvPercept.AttributeAgentLocation,
                    VacuumEnvironment.LocationA),
                    VacuumEnvironment.ActionMoveRight));
            rules.Add(new Rule(new EQUALCondition(
                    VacuumEnvPercept.AttributeAgentLocation,
                    VacuumEnvironment.LocationB),
                    VacuumEnvironment.ActionMoveLeft));

            return rules;
        }
    }

}
