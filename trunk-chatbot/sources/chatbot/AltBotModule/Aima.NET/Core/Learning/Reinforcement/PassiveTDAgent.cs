using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Reinforcement
{
    using Aima.Core.Probability.Decision;
    using Aima.Core.Util;

    public class PassiveTDAgent<TState, TAction> : MDPAgent<TState, TAction> 
        where TState : class 
        where TAction : class
    {
        private MDPPolicy<TState, TAction> policy;

        private MDPUtilityFunction<TState> utilityFunction;

        // private Hashtable<STATE_TYPE,Double> stateCount;
        private FrequencyCounter<TState> stateCount;

        private double previousReward;

        public PassiveTDAgent(MDP<TState, TAction> mdp,
                MDPPolicy<TState, TAction> policy) : base(mdp.EmptyMdp())
        {
            this.policy = policy;
            this.utilityFunction = new MDPUtilityFunction<TState>();
            this.stateCount = new FrequencyCounter<TState>();
        }

        public override TAction DecideAction(MDPPerception<TState> perception) 
        {

            if (!(utilityFunction.HasUtilityFor(perception.GetState()))) 
            { // if
                // perceptionState
                // is
                // new
                utilityFunction.SetUtility(perception.GetState(), perception.GetReward());
                MDP.SetReward(perception.GetState(), perception.GetReward());
            }
            if (!(PreviousState == null)) {
                stateCount.IncrementFor(PreviousState);
                utilityFunction = this.UpdateUtilityFunction(1.0);
            }

            if (MDP.IsTerminalState(CurrentState)) {
                PreviousState = null;
                PreviousAction = null;
                //TODO: make sure that 0 is appropriate value for what used to be null in java
                previousReward = 0;
            } else {
                PreviousState = CurrentState;
                PreviousAction = policy.GetAction(CurrentState);
                previousReward = CurrentReward;
            }
            return PreviousAction;
        }

        public MDPUtilityFunction<TState> GetUtilityFunction() {
            return utilityFunction;
        }

        private MDPUtilityFunction<TState> UpdateUtilityFunction(double gamma) 
        {
            MDPUtilityFunction<TState> uf = utilityFunction.Copy();
            double u_s = utilityFunction.GetUtility(PreviousState);
            double gammaUtilDIff = ((gamma * utilityFunction
                    .GetUtility(CurrentState)) - utilityFunction
                    .GetUtility(PreviousState));
            double alphaTerm = stateCount.ProbabilityOf(PreviousState)
                    * (previousReward + gammaUtilDIff);
            uf.SetUtility(PreviousState, u_s + alphaTerm);
            return uf;
        }
    }
}
