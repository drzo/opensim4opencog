using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Reinforcement
{
    using Aima.Core.Probability;
    using Aima.Core.Probability.Decision;

    public abstract class MDPAgent<TState, TAction> 
        where TAction:class
        where TState:class
    {

        protected MDP<TState, TAction> MDP;

        protected TState CurrentState;

        protected double CurrentReward;

        protected TState PreviousState;

        protected TAction PreviousAction;

        public MDPAgent(MDP<TState, TAction> mdp)
        {
            this.MDP = mdp;
            this.CurrentState = mdp.GetInitialState();
            this.CurrentReward = mdp.GetRewardFor(this.CurrentState);

        }

        public MDPPerception<TState> Execute(TAction action, IRandomizer r)
        {
            MDPPerception<TState> perception = this.MDP.Execute(this.CurrentState, action, r);
            this.UpdateFromPerception(perception);
            return perception;
        }

        public void UpdateFromPerception(MDPPerception<TState> perception)
        {
            this.CurrentState = perception.GetState();
            this.CurrentReward = perception.GetReward();
        }

        public void ExecuteTrial(IRandomizer r)
        {
            this.CurrentState = this.MDP.GetInitialState();
            this.CurrentReward = this.MDP.GetRewardFor(this.MDP.GetInitialState());
            this.PreviousState = null;
            this.PreviousAction = null;
            MDPPerception<TState> perception = new MDPPerception<TState>(
                    this.CurrentState, this.CurrentReward);
            TAction action = null;
            do
            {
                action = this.DecideAction(perception);
                if (action != null)
                {
                    perception = this.Execute(action, r);
                }
            } while (action != null);
        }

        public abstract TAction DecideAction(
                MDPPerception<TState> perception);

        public double GetCurrentReward()
        {
            return this.CurrentReward;
        }

        public void SetCurrentReward(double currentReward)
        {
            this.CurrentReward = currentReward;
        }

        public TAction GetPreviousAction()
        {
            return this.PreviousAction;
        }

        public void SetPreviousAction(TAction previousAction)
        {
            this.PreviousAction = previousAction;
        }

        public TState GetPreviousState()
        {
            return this.PreviousState;
        }

        public void SetPreviousState(TState previousState)
        {
            this.PreviousState = previousState;
        }

        public TState GetCurrentState()
        {
            return this.CurrentState;
        }
    }
}
