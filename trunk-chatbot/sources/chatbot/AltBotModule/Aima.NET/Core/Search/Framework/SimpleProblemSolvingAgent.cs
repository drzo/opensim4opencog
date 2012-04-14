using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.1, page 67.
    /// <code><![CDATA[
    /// function SIMPLE-PROBLEM-SOLVING-AGENT(percept) returns an action
    ///   persistent: seq, an action sequence, initially empty
    ///               state, some description of the current world state
    ///               goal, a goal, initially null
    ///               problem, a problem formulation
    ///           
    ///   state <- UPDATE-STATE(state, percept)
    ///   if seq is empty then
    ///     goal    <- FORMULATE-GOAL(state)
    ///     problem <- FORMULATE-PROBLEM(state, goal)
    ///     seq     <- SEARCH(problem)
    ///     if seq = failure then return a null action
    ///   action <- FIRST(seq)
    ///   seq <- REST(seq)
    ///   return action
    /// ]]></code>
    /// Figure 3.1 A simple problem-solving agent. It first formulates a goal and a problem,
    /// searches for a sequence of actions that would solve the problem, and then executes the actions
    /// one at a time. When this is complete, it formulates another goal and starts over.
    /// </summary>
    public abstract class SimpleProblemSolvingAgent : AbstractAgent {

        /// <summary>
        /// seq, an action sequence, initially empty
        /// </summary>
        private IList<IAction> seq = new List<IAction>();

        private bool formulateGoalsIndefinitely = true;

        private int maxGoalsToFormulate = 1;

        private int goalsFormulated;

        public SimpleProblemSolvingAgent() 
        {
            formulateGoalsIndefinitely = true;
        }

        public SimpleProblemSolvingAgent(int maxGoalsToFormulate) 
        {
            formulateGoalsIndefinitely = false;
            this.maxGoalsToFormulate = maxGoalsToFormulate;
        }

        /// <summary>
        /// function SIMPLE-PROBLEM-SOLVING-AGENT(percept) returns an action
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public override IAction Execute(IPercept p)
        {
            IAction action = NoOpAction.NoOp;

            // state <- UPDATE-STATE(state, percept)
            this.UpdateState(p);

            // if seq is empty then do
            if (0 == this.seq.Count)
            {
                if (this.formulateGoalsIndefinitely || this.goalsFormulated < this.maxGoalsToFormulate)
                {
                    if (this.goalsFormulated > 0)
                    {
                        this.NotifyViewOfMetrics();
                    }

                    // goal <- FORMULATE-GOAL(state)
                    var goal = this.FormulateGoal();
                    this.goalsFormulated++;

                    // problem <- FORMULATE-PROBLEM(state, goal)
                    var problem = this.FormulateProblem(goal);

                    // seq <- SEARCH(problem)
                    seq = this.Search(problem);
                    if (this.seq.Count == 0)
                    {
                        // Unable to identify a path
                        seq.Add(NoOpAction.NoOp);
                    }
                }
                else
                {
                    // Agent no longer wishes to
                    // achieve any more goals
                    Alive = false;
                    this.NotifyViewOfMetrics();
                }
            }

            if (this.seq.Count > 0)
            {
                // action <- FIRST(seq)
                action = Util.Util.First(this.seq);

                // seq <- REST(seq)
                this.seq = Util.Util.Rest(this.seq);
            }

            return action;
        }

        /// <summary>
        /// </summary>
        /// <param name="p">
        /// The p.
        /// </param>
        /// <returns>
        /// </returns>
        protected abstract IState UpdateState(IPercept p);

        protected abstract object FormulateGoal();

        protected abstract Problem FormulateProblem(object goal);

        protected abstract IList<IAction> Search(Problem problem);

        protected abstract void NotifyViewOfMetrics();
    }
}
