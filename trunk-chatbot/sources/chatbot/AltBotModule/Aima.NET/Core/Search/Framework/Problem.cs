using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 66.
    /// 
    /// A problem can be defined formally by five components: <br>
    /// <ul>
    /// <li>The <b>initial state</b> that the agent starts in.</li>
    /// <li>A description of the possible <b>actions</b> available to the agent. 
    /// Given a particular state s, ACTIONS(s) returns the set of actions that can 
    /// be executed in s.</li>
    /// <li>A description of what each action does; the formal name for this is the 
    /// <b>transition model, specified by a function RESULT(s, a) that returns the 
    /// state that results from doing action a in state s.</b></li>
    /// <li>The <b>goal test</b>, which determines whether a given state is a goal 
    /// state.</li>
    /// <li>A <b>path cost</b> function that assigns a numeric cost to each path. 
    /// The problem-solving agent chooses a cost function that reflects its own 
    /// performance measure. The <b>step cost</b> of taking action a in state s 
    /// to reach state s' is denoted by c(s,a,s')</li>
    /// </ul>
    /// </summary>
    public class Problem
    {
        /// <summary>
        /// </summary>
        public object InitialState { get; protected set; }

        public IActionsFunction ActionsFunction { get; protected set; }

        public IResultFunction ResultFunction { get; protected set; }

        public IGoalTest GoalTest { get; protected set; }

        public IStepCostFunction StepCostFunction { get; protected set; }

        public Problem(object initialState, IActionsFunction actionsFunction,
                IResultFunction resultFunction, IGoalTest goalTest)
            : this(initialState, actionsFunction, resultFunction, goalTest,
                new DefaultStepCostFunction())
        {
        }

        public Problem(object initialState, IActionsFunction actionsFunction,
                IResultFunction resultFunction, IGoalTest goalTest,
                IStepCostFunction stepCostFunction)
        {
            this.InitialState = initialState;
            this.ActionsFunction = actionsFunction;
            this.ResultFunction = resultFunction;
            this.GoalTest = goalTest;
            this.StepCostFunction = stepCostFunction;
        }

        public bool IsGoalState(object state)
        {
            return this.GoalTest.IsGoalState(state);
        }

        protected Problem()
        {
        }
    }
}
