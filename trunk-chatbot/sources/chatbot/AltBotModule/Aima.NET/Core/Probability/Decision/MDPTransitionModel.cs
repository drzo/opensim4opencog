using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    using Aima.Core.Util.Datastructure;

    //TODO: all these MDP... classes can probably be placed in separate MDP namespace
    public class MDPTransitionModel<TState, TAction> where TAction : class
    {

        private Dictionary<MDPTransition<TState, TAction>, double> transitionToProbability = new Dictionary<MDPTransition<TState, TAction>, double>();

        private IList<TState> terminalStates;

        public MDPTransitionModel(IList<TState> terminalStates) {
            this.terminalStates = terminalStates;

        }

        public void SetTransitionProbability(TState initialState,
                TAction action, TState finalState, double probability) {
            if (!(IsTerminal(initialState))) {
                MDPTransition<TState, TAction> t = new MDPTransition<TState, TAction>(
                        initialState, action, finalState);
                transitionToProbability[t] = probability;
            }
        }

        public double GetTransitionProbability(TState initialState,
                TAction action, TState finalState) {
            MDPTransition<TState, TAction> key = new MDPTransition<TState, TAction>(
                    initialState, action, finalState);
            if (transitionToProbability.Keys.Contains(key)) {
                return transitionToProbability[key];
            } else {
                return 0.0;
            }
        }

        public override string ToString()
        {
            return transitionToProbability.Keys.Aggregate(
                new StringBuilder(),
                (sb, transition) => sb.Append(String.Format("{0} -> {1} \n", transition, transitionToProbability[transition])),
                (sb) => sb.ToString());
        }

        public Pair<TAction, double> GetTransitionWithMaximumExpectedUtility(
                TState s, MDPUtilityFunction<TState> uf) {

            if ((IsTerminal(s))) {
                return new Pair<TAction, Double>(null, 0.0);
            }

            var transitionsStartingWithS = this.GetTransitionsStartingWith(s);
            Dictionary<TAction, double> actionsToUtilities = GetExpectedUtilityForSelectedTransitions(transitionsStartingWithS, uf);

            return GetActionWithMaximumUtility(actionsToUtilities);

        }

        public Pair<TAction, double> GetTransitionWithMaximumExpectedUtilityUsingPolicy(
                MDPPolicy<TState, TAction> policy, TState s,
                MDPUtilityFunction<TState> uf) 
        {
            if ((IsTerminal(s))) {
                return new Pair<TAction, Double>(null, 0.0);
            }
            var transitionsWithStartingStateSAndActionFromPolicy = this.GetTransitionsWithStartingStateAndAction(
                    s, policy.GetAction(s));
            Dictionary<TAction, Double> actionsToUtilities = GetExpectedUtilityForSelectedTransitions(
                    transitionsWithStartingStateSAndActionFromPolicy, uf);

            return this.GetActionWithMaximumUtility(actionsToUtilities);

        }

        public IList<MDPTransition<TState, TAction>> GetTransitionsWithStartingStateAndAction(
                TState s, TAction a) {
            return transitionToProbability.Keys.Where(transition => (transition.GetInitialState().Equals(s)) && (transition.GetAction().Equals(a))).ToList();
        }

        public TAction RandomActionFor(TState s) {
            var transitions = this.GetTransitionsStartingWith(s);
            
            // TODO: figure out what to do with this commented out code
            // MDPTransition<STATE_TYPE, ACTION_TYPE> randomTransition = Util
            // .selectRandomlyFromList(transitions);
            return transitions[0].GetAction();
            // return randomTransition.getAction();
        }

        private bool IsTerminal(TState s) {
            return terminalStates.Contains(s);
        }

        private Pair<TAction, double> GetActionWithMaximumUtility(
                Dictionary<TAction, double> actionsToUtilities) {
            var highest = new Pair<TAction, Double>(null,
                    double.MinValue);
            foreach (TAction key in actionsToUtilities.Keys) {
                var value = actionsToUtilities[key];
                if (value > highest.GetSecond()) {
                    highest = new Pair<TAction, Double>(key, value);
                }
            }
            return highest;
        }

        private Dictionary<TAction, Double> GetExpectedUtilityForSelectedTransitions(
        IList<MDPTransition<TState, TAction>> transitions,
                MDPUtilityFunction<TState> uf) {
            var actionsToUtilities = new Dictionary<TAction, double>();
            foreach (var triplet in transitions) {
                var s = triplet.GetInitialState();
                var action = triplet.GetAction();
                var destinationState = triplet.GetDestinationState();
                var probabilityOfTransition = this.GetTransitionProbability(s,
                        action, destinationState);
                var expectedUtility = (probabilityOfTransition * uf
                        .GetUtility(destinationState));
                
                if (!actionsToUtilities.ContainsKey(action)) 
                {
                    actionsToUtilities[action] = expectedUtility;
                } else {
                    actionsToUtilities[action] = actionsToUtilities[action] + expectedUtility;
                }
            }
            return actionsToUtilities;
        }

        private IList<MDPTransition<TState, TAction>> GetTransitionsStartingWith(
                TState s) {
            IList<MDPTransition<TState, TAction>> result = new List<MDPTransition<TState, TAction>>();
            foreach (MDPTransition<TState, TAction> transition in transitionToProbability.Keys) {
                if (transition.GetInitialState().Equals(s)) {
                    result.Add(transition);
                }
            }
            return result;
        }
    }
}
