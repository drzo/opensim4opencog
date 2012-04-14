using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Reinforcement
{
    using Aima.Core.Probability.Decision;
    using Aima.Core.Util.Datastructure;

    public class PassiveADPAgent<TState, TAction> : MDPAgent<TState, TAction>
        where TState : class 
        where TAction:class
    {
        private MDPPolicy<TState, TAction> policy;

        private MDPUtilityFunction<TState> utilityFunction;

        private Dictionary<Pair<TState, TAction>, double> nsa;

        private Dictionary<MDPTransition<TState, TAction>, double> nsasdash;

        public PassiveADPAgent(MDP<TState, TAction> mdp,
                MDPPolicy<TState, TAction> policy) : base(mdp.EmptyMdp())
        {
            this.policy = policy;
            this.utilityFunction = new MDPUtilityFunction<TState>();
            this.nsa = new Dictionary<Pair<TState, TAction>, double>();
            this.nsasdash = new Dictionary<MDPTransition<TState, TAction>, double>();

        }

        public override TAction DecideAction(MDPPerception<TState> perception) 
        {
            if (!(utilityFunction.HasUtilityFor(perception.GetState()))) 
            { // if
                // perceptionState
                // is
                // new
                utilityFunction.SetUtility(perception.GetState(), perception
                        .GetReward());
                MDP.SetReward(perception.GetState(), perception.GetReward());
            }
            if (!(PreviousState == null))
            {
                if (nsa.ContainsKey(new Pair<TState, TAction>(
                        PreviousState, PreviousAction)))
                {
                    nsa[new Pair<TState, TAction>(PreviousState, PreviousAction)] += 1;
                }
                else
                {
                    nsa[new Pair<TState, TAction>(PreviousState, PreviousAction)] = 1.0;
                }
                if (nsasdash.ContainsKey(new MDPTransition<TState, TAction>(PreviousState, PreviousAction, CurrentState)))
                {
                    nsasdash[new MDPTransition<TState, TAction>(PreviousState, PreviousAction, CurrentState)] += 1;
                }
                else
                {
                    nsasdash[new MDPTransition<TState, TAction>(PreviousState, PreviousAction, CurrentState)] = 1.0;
                }
                
                foreach (MDPTransition<TState, TAction> transition in nsasdash.Keys) 
                {
                    if (nsasdash[transition] != 0.0) 
                    {
                        double newValue = nsasdash[transition]
                                / nsa[new Pair<TState, TAction>(
                                        transition.GetInitialState(), transition.GetAction())];
                        MDP.SetTransitionProbability(transition, newValue);
                    }
                }
                IList<MDPTransition<TState, TAction>> validTransitions = MDP
                        .GetTransitionsWith(PreviousState, policy.GetAction(PreviousState));
                utilityFunction = this.ValueDetermination(validTransitions, 1);
            }

            if (MDP.IsTerminalState(CurrentState)) {
                PreviousState = null;
                PreviousAction = null;
            } else {
                PreviousState = CurrentState;
                PreviousAction = policy.GetAction(CurrentState);
            }
            return PreviousAction;
        }

        public MDPUtilityFunction<TState> GetUtilityFunction() {
            return utilityFunction;
        }

        private MDPUtilityFunction<TState> ValueDetermination(
                IList<MDPTransition<TState, TAction>> validTransitions,
                double gamma)
        {
            MDPUtilityFunction<TState> uf = utilityFunction.Copy();
            double additional = 0.0;
            if (validTransitions.Count > 0)
            {
                TState initState = validTransitions[0].GetInitialState();
                double reward = MDP.GetRewardFor(initState);
                additional =
                    validTransitions.Sum(
                        transition =>
                        MDP.GetTransitionProbability(transition) *
                        this.utilityFunction.GetUtility(transition.GetDestinationState()));

                uf.SetUtility(initState, reward + (gamma * additional));
            }

            return uf;
        }
    }
}
