using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Agent.Impl.AProg
{
    using Aima.Core.Agent.Impl.AProg.SimpleRule;

    public abstract class ModelBasedReflexAgentProgram : IAgentProgram 
    {
        //
        // persistent: state, the agent's current conception of the world state
        private DynamicState state = null;

        // model, a description of how the next state depends on current state and
        // action
        private IModel model = null;

        // rules, a set of condition-action rules
        private ISet<Rule> rules = null;

        // action, the most recent action, initially none
        private IAction action = null;

        public ModelBasedReflexAgentProgram() 
        {
            //TODO: do something with this virtual method call to make sure that the object is initialized correctly
            Init();
        }

        public void SetState(DynamicState aState) 
        {
            state = aState;
        }

        public void SetModel(IModel aModel) 
        {
            model = aModel;
        }

        public void SetRules(ISet<Rule> aRuleSet)
        {
            rules = aRuleSet;
        }

        // function MODEL-BASED-REFLEX-AGENT(percept) returns an action
        public IAction Execute(IPercept percept) 
        {
            this.state = this.UpdateState(this.state, this.action, percept, this.model);

            var rule = this.RuleMatch(this.state, this.rules);

            this.action = this.RuleAction(rule);

            return this.action;
        }

        /**
         * Realizations of this class should implement the init() method so that it
         * calls the setState(), setModel(), and setRules() method.
         */
        protected abstract void Init();

        protected abstract DynamicState UpdateState(DynamicState state,
                IAction action, IPercept percept, IModel model);

        protected Rule RuleMatch(DynamicState state, ISet<Rule> rules) 
        {
            foreach (Rule r in rules) 
            {
                if (r.Evaluate(state)) 
                {
                    return r;
                }
            }
            return null;
        }

        protected IAction RuleAction(Rule r) 
        {
            return null == r ? NoOpAction.NoOp : r.GetAction();
        }
    }
}   
