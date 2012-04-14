using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Online
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 147.
    /// 
    /// An online search problem must be solved by an agent executing actions, 
    /// rather than by pure computation. We assume a deterministic and fully
    /// observable environment (Chapter 17 relaxes these assumptions), but we
    /// stipulate that the agent knows only the following: <br />
    /// <ul>
    /// <li>ACTIONS(s), which returns a list of actions allowed in state s;</li>
    /// <li>The step-cost function c(s, a, s') - note that this cannot be used
    /// until the agent knows that s' is the outcome; and</li>
    /// <li>GOAL-TEST(s).</li>
    /// </ul>
    /// </summary>
    public class OnlineSearchProblem
    {

        protected IActionsFunction ActionsFunction;

        public IStepCostFunction StepCostFunction { get; protected set; }

        protected IGoalTest GoalTest;

        public OnlineSearchProblem(IActionsFunction actionsFunction, IGoalTest goalTest)
            : this(actionsFunction, goalTest, new DefaultStepCostFunction())
        {
        }

        public OnlineSearchProblem(IActionsFunction actionsFunction,
                IGoalTest goalTest, IStepCostFunction stepCostFunction)
        {
            this.ActionsFunction = actionsFunction;
            this.GoalTest = goalTest;
            this.StepCostFunction = stepCostFunction;
        }

        public IActionsFunction getActionsFunction()
        {
            return this.ActionsFunction;
        }

        public bool IsGoalState(object state)
        {

            return this.GoalTest.IsGoalState(state);
        }

        protected OnlineSearchProblem()
        {
        }
    }
}
