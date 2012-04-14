using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Search.Online
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach 3rdd Edition): Figure 4.24, page 152.<br>
    /// <code><![CDATA[
    /// function LRTA*-AGENT(s') returns an action
    ///   inputs: s', a percept that identifies the current state
    ///   persistent: result, a table, indexed by state and action, initially empty
    ///               H, a table of cost estimates indexed by state, initially empty
    ///               s, a, the previous state and action, initially null
    ///           
    ///   if GOAL-TEST(s') then return stop
    ///   if s' is a new state (not in H) then H[s'] <- h(s')
    ///   if s is not null
    ///     result[s, a] <- s'
    ///     H[s] <-        min LRTA*-COST(s, b, result[s, b], H)
    ///             b (element of) ACTIONS(s)
    ///   a <- an action b in ACTIONS(s') that minimizes LRTA*-COST(s', b, result[s', b], H)
    ///   s <- s'
    ///   return a
    ///   
    /// function LRTA*-COST(s, a, s', H) returns a cost estimate
    ///   if s' is undefined then return h(s)
    ///   else return c(s, a, s') + H[s']
    /// ]]></code>
    /// 
    /// Figure 4.24 LRTA*-AGENT selects an action according to the value of
    /// neighboring states, which are updated as the agent moves about the state
    /// space.<br />
    /// Note: This algorithm fails to exit if the goal does not exist (e.g. A<->B Goal=X),
    /// this could be an issue with the implementation. Comments welcome.
    /// </summary>
    public class LRTAStarAgent : AbstractAgent 
    {

        private OnlineSearchProblem problem;

        public IPerceptToStateFunction PerceptToStateFunction { get; set; }

        public IHeuristicFunction HeuristicFunction { get; set; }

        // persistent: result, a table, indexed by state and action, initially empty
        private readonly Dictionary<StateAction, object> result = new Dictionary<StateAction, object>();
        // H, a table of cost estimates indexed by state, initially empty
        private readonly Dictionary<object, double> h = new Dictionary<object, double>();
        // s, a, the previous state and action, initially null
        private object s;
        private IAction a;

        public LRTAStarAgent(OnlineSearchProblem problem, IPerceptToStateFunction ptsFunction, IHeuristicFunction hf) 
        {
            this.Problem = problem;
            this.PerceptToStateFunction = ptsFunction;
            this.HeuristicFunction = hf;
        }

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

        // function LRTA*-AGENT(s') returns an action
        // inputs: s', a percept that identifies the current state
        public override IAction Execute(IPercept psPrime) 
        {
            object sPrime = this.PerceptToStateFunction.GetState(psPrime);
            // if GOAL-TEST(s') then return stop
            if (this.GoalTest(sPrime)) 
            {
                a = NoOpAction.NoOp;
            } else
            {
                // if s' is a new state (not in H) then H[s'] <- h(s')
                if (!this.h.ContainsKey(sPrime))
                {
                    this.h[sPrime] = this.HeuristicFunction.H(sPrime);
                }

                // if s is not null
                if (null != this.s)
                {
                    // result[s, a] <- s'
                    this.result[new StateAction(this.s, this.a)] = sPrime;

                    // H[s] <- min LRTA*-COST(s, b, result[s, b], H)
                    // b (element of) ACTIONS(s)
                    double minimum = double.MaxValue;
                    foreach (IAction b in this.Actions(this.s))
                    {
                        double cost = this.LRTACost(this.s, b, this.result[new StateAction(this.s, b)]);
                        if (cost < minimum)
                        {
                            minimum = cost;
                        }
                    }
                    this.h[this.s] = minimum;
                }

                // a <- an action b in ACTIONS(s') that minimizes LRTA*-COST(s', b,
                // result[s', b], H)
                double min = double.MaxValue;

                // Just in case no actions
                this.a = NoOpAction.NoOp;
                foreach (IAction b in this.Actions(sPrime))
                {
                    double cost = this.LRTACost(sPrime, b, this.result[new StateAction(sPrime, b)]);
                    if (cost < min)
                    {
                        min = cost;
                        this.a = b;
                    }
                }
            }

            // s <- s'
            this.s = sPrime;

            if (this.a.IsNoOp())
            {
                // I'm either at the Goal or can't get to it,
                // which in either case I'm finished so just die.
                Alive = false;
            }

            // return a
            return this.a;
        }

        private void Init() {
            Alive = true;
            result.Clear();
            this.h.Clear();
            s = null;
            a = null;
        }

        private bool GoalTest(object state) 
        {
            return this.Problem.IsGoalState(state);
        }

        // function LRTA*-COST(s, a, s', H) returns a cost estimate
        private double LRTACost(object s, IAction action, object sPrime) 
        {
            // if s' is undefined then return h(s)
            if (null == sPrime) 
            {
                return HeuristicFunction.H(s);
            }
            // else return c(s, a, s') + H[s']
            return Problem.StepCostFunction.C(s, action, sPrime) + this.h[sPrime];
        }

        private ISet<IAction> Actions(object state) 
        {
            return this.Problem.getActionsFunction().Actions(state);
        }
    }

}
