using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

using Aima.Core.Agent;
using Aima.Core.Agent.Impl;
using Aima.Core.Agent.Impl.AProg.SimpleRule;

namespace Aima.Core.Agent.Impl.AProg
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.10, page 49.
    /// <code><![CDATA[
    /// function SIMPLE-RELEX-AGENT(percept) returns an action
    ///  persistent: rules, a set of condition-action rules
    ///   
    ///   state  <- INTERPRET-INPUT(percept);
    ///   rule   <- RULE-MATCH(state, rules);
    ///   action <- rule.ACTION;
    ///   return action
    /// 
    /// ]]></code>
    /// Figure 2.10 A simple reflex agent. It acts according to a rule whose condition matches
    /// the current state, as defined by the percept.
    /// </summary>
    public class SimpleReflexAgentProgram : IAgentProgram 
    {
	    //
	    // persistent: rules, a set of condition-action rules
	    private ISet<Rule> rules;

	    public SimpleReflexAgentProgram(ISet<Rule> aRuleSet) {
		    rules = aRuleSet;
	    }

	    // function SIMPLE-RELEX-AGENT(percept) returns an action
	    public IAction Execute(IPercept percept) {

		    // state <- INTERPRET-INPUT(percept);
		    var state = InterpretInput(percept);
		    // rule <- RULE-MATCH(state, rules);
		    var rule = RuleMatch(state, rules);
		    // action <- rule.ACTION;
		    // return action
		    return ruleAction(rule);
	    }

	    protected ObjectWithDynamicAttributes InterpretInput(IPercept p) {
		    return (DynamicPercept) p;
	    }

	    protected Rule RuleMatch(ObjectWithDynamicAttributes state,
			    ISet<Rule> rulesSet) {
		    foreach (Rule r in rulesSet) {
			    if (r.Evaluate(state)) {
				    return r;
			    }
		    }
		    return null;
	    }

	    protected IAction ruleAction(Rule r) {
		    return null == r ? NoOpAction.NoOp : r.GetAction();
	    }
    }
}
