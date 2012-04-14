using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Online
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.21, page 150.
    /// <code><![CDATA[
    /// function ONLINE-DFS-AGENT(s') returns an action
    ///   inputs: s', a percept that identifies the current state
    ///   persistent: result, a table, indexed by state and action, initially empty
    ///               untried, a table that lists, for each state, the actions not yet tried
    ///               unbacktracked, a table that lists, for each state, the backtracks not yet tried
    ///               s, a, the previous state and action, initially null
    ///    
    ///   if GOAL-TEST(s') then return stop
    ///   if s' is a new state (not in untried) then untried[s'] <- ACTIONS(s')
    ///   if s is not null then
    ///       result[s, a] <- s'
    ///       add s to the front of the unbacktracked[s']
    ///   if untried[s'] is empty then
    ///       if unbacktracked[s'] is empty then return stop
    ///       else a <- an action b such that result[s', b] = POP(unbacktracked[s'])
    ///   else a <- POP(untried[s'])
    ///   s <- s'
    ///   return a
    /// ]]></code>
    /// Figure 4.21 An online search agent that uses depth-first exploration. The agent is
    /// applicable only in state spaces in which every action can be "undone" by some other action.<br>
    /// </summary>
    public class OnlineDFSAgent : AbstractAgent 
    {

        private OnlineSearchProblem problem;

        public OnlineSearchProblem Problem
        {
            get
            {
                return this.problem;
            }
            set
            {
                this.problem = value;
                this.Init();
            }
        }

        public IPerceptToStateFunction PerceptToStateFunction { get; set; }
        // persistent: result, a table, indexed by state and action, initially empty
        private readonly IDictionary<StateAction, object> result = new Dictionary<StateAction, object>();
        // untried, a table that lists, for each state, the actions not yet tried
        private readonly IDictionary<object, IList<IAction>> untried = new Dictionary<object, IList<IAction>>();
        // unbacktracked, a table that lists,
        // for each state, the backtracks not yet tried
        private readonly IDictionary<object, IList<object>> unbacktracked = new Dictionary<object, IList<object>>();
        // s, a, the previous state and action, initially null
        private object s;
        private IAction a;

        public OnlineDFSAgent(OnlineSearchProblem problem,
                IPerceptToStateFunction ptsFunction) {
            Problem = problem;
            PerceptToStateFunction = ptsFunction;
        }

        // function ONLINE-DFS-AGENT(s') returns an action
        // inputs: s', a percept that identifies the current state
        public override IAction Execute(IPercept psPrime)
        {
            object sPrime = this.PerceptToStateFunction.GetState(psPrime);
            // if GOAL-TEST(s') then return stop
            if (this.GoalTest(sPrime))
            {
                a = NoOpAction.NoOp;
            }
            else
            {
                // if s' is a new state (not in untried) then untried[s'] <-
                // ACTIONS(s')
                if (!untried.ContainsKey(sPrime))
                {
                    untried[sPrime] = this.Actions(sPrime);
                }

                // if s is not null then do
                if (null != s)
                {
                    var sa = new StateAction(s, a);
                    // Note: If I've already seen the result of this
                    // [s, a] then don't put it back on the unbacktracked
                    // list otherwise you can keep oscillating
                    // between the same states endlessly.
                    if (!(sPrime.Equals(result[sa])))
                    {
                        // result[s, a] <- s'
                        result[sa] = sPrime;

                        // Ensure the unbacktracked always has a list for s'
                        if (!unbacktracked.ContainsKey(sPrime))
                        {
                            unbacktracked[sPrime] = new List<object>();
                        }

                        // add s to the front of the unbacktracked[s']
                        unbacktracked[sPrime].Insert(0, s);
                    }
                }
                // if untried[s'] is empty then
                if (untried[sPrime].Count == 0)
                {
                    // if unbacktracked[s'] is empty then return stop
                    if (unbacktracked[sPrime].Count == 0)
                    {
                        a = NoOpAction.NoOp;
                    }
                    else
                    {
                        // else a <- an action b such that result[s', b] =
                        // POP(unbacktracked[s'])
                        object popped = this.unbacktracked[sPrime][0];
                        this.unbacktracked[sPrime].RemoveAt(0);
                        foreach (StateAction sa in this.result.Keys)
                        {
                            if (sa.State.Equals(sPrime) && this.result[sa].Equals(popped))
                            {
                                this.a = sa.Action;
                                break;
                            }
                        }
                    }
                }
                else
                {
                    // else a <- POP(untried[s'])
                    this.a = this.untried[sPrime][0];
                    this.untried[sPrime].RemoveAt(0);
                }
            }

            if (this.a.IsNoOp())
            {
                // I'm either at the Goal or can't get to it,
                // which in either case I'm finished so just die.
                Alive = false;
            }

            // s <- s'
            this.s = sPrime;

            // return a
            return this.a;
        }

        private void Init() 
        {
            Alive = true;
            result.Clear();
            untried.Clear();
            unbacktracked.Clear();
            s = null;
            a = null;
        }

        private bool GoalTest(object state) 
        {
            return this.Problem.IsGoalState(state);
        }

        private IList<IAction> Actions(object state) 
        {
            return new List<IAction>(problem.getActionsFunction().Actions(state));
        }
    }

}
