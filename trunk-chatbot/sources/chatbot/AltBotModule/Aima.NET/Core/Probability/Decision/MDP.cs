using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    using Aima.Core.Util.Datastructure;

    public class MDP<TState, TAction> where TAction : class
{
    private TState initialState;

    private MDPTransitionModel<TState, TAction> transitionModel;

    private MDPRewardFunction<TState> rewardFunction;

    private IList<TState> nonFinalstates, terminalStates;

    private IMDPSource<TState, TAction> source;

    public MDP(IMDPSource<TState, TAction> source) {
        this.initialState = source.GetInitialState();
        this.transitionModel = source.GetTransitionModel();
        this.rewardFunction = source.GetRewardFunction();
        this.nonFinalstates = source.GetNonFinalStates();
        this.terminalStates = source.GetFinalStates();
        this.source = source;
    }

    public MDP<TState, TAction> EmptyMdp() {
        MDP<TState, TAction> mdp = new MDP<TState, TAction>(
                source);
        mdp.rewardFunction = new MDPRewardFunction<TState>();
        mdp.rewardFunction.SetReward(initialState, rewardFunction
                .GetRewardFor(initialState));
        mdp.transitionModel = new MDPTransitionModel<TState, TAction>(
                terminalStates);
        return mdp;
    }

    public MDPUtilityFunction<TState> ValueIteration(double gamma,
            double error, double delta) {
        MDPUtilityFunction<TState> u;
        var uDash = this.InitialUtilityFunction();
        var deltaMax = (error * gamma) / (1 - gamma);
        do {
            u = uDash.Copy();
            // System.out.println(U);
            delta = 0.0;
            foreach (var s in nonFinalstates) {
                var highestUtilityTransition = transitionModel
                        .GetTransitionWithMaximumExpectedUtility(s, u);
                var utility = rewardFunction.GetRewardFor(s)
                        + (gamma * highestUtilityTransition.GetSecond());
                uDash.SetUtility(s, utility);
                if ((Math.Abs(uDash.GetUtility(s) - u.GetUtility(s))) > delta) {
                    delta = Math.Abs(uDash.GetUtility(s) - u.GetUtility(s));
                }

            }
        } while (delta < deltaMax);
        return u;

    }

    public MDPUtilityFunction<TState> ValueIterationForFixedIterations(
            int numberOfIterations, double gamma) {
        var utilityFunction = this.InitialUtilityFunction();

        for (var i = 0; i < numberOfIterations; i++) {
            var result = this.ValueIterateOnce(
                    gamma, utilityFunction);
            utilityFunction = result.GetFirst();
            // TODO: figure out what to do with commented out code
            // double maxUtilityGrowth = result.getSecond();
            // System.out.println("maxUtilityGrowth " + maxUtilityGrowth);
        }

        return utilityFunction;
    }

    public MDPUtilityFunction<TState> ValueIterationTillMAximumUtilityGrowthFallsBelowErrorMargin(
            double gamma, double errorMargin) {
        var iterationCounter = 0;
        var maxUtilityGrowth = 0.0;
        var utilityFunction = this.InitialUtilityFunction();
        do {
            var result = this.ValueIterateOnce(
                    gamma, utilityFunction);
            utilityFunction = result.GetFirst();
            maxUtilityGrowth = result.GetSecond();
            iterationCounter++;
            // TODO: add debug code 
            // System.out.println("Itration Number" +iterationCounter + " max
            // utility growth " + maxUtilityGrowth);

        } while (maxUtilityGrowth > errorMargin);

        return utilityFunction;
    }

    public Pair<MDPUtilityFunction<TState>, Double> ValueIterateOnce(
            double gamma, MDPUtilityFunction<TState> presentUtilityFunction) {
        var maxUtilityGrowth = 0.0;
        var newUtilityFunction = new MDPUtilityFunction<TState>();

        foreach (TState s in nonFinalstates) {
            // TODO: figure out what to do with commented out code
            // double utility = rewardFunction.getRewardFor(s)
            // + (gamma * highestUtilityTransition.getSecond());

            var utility = this.ValueIterateOnceForGivenState(gamma,
                    presentUtilityFunction, s);

            var differenceInUtility = Math.Abs(utility
                    - presentUtilityFunction.GetUtility(s));
            if (differenceInUtility > maxUtilityGrowth) {
                maxUtilityGrowth = differenceInUtility;
            }
            newUtilityFunction.SetUtility(s, utility);

            foreach (var state in terminalStates) {
                newUtilityFunction.SetUtility(state, presentUtilityFunction
                        .GetUtility(state));
            }
        }

        return new Pair<MDPUtilityFunction<TState>, Double>(
                newUtilityFunction, maxUtilityGrowth);

    }

    public MDPPolicy<TState, TAction> PolicyIteration(double gamma) {
        var U = this.InitialUtilityFunction();
        var pi = this.RandomPolicy();
        var unchanged = false;
        do {
            unchanged = true;

            U = this.PolicyEvaluation(pi, U, gamma, 3);
            foreach (var s in nonFinalstates) {
                var maxTransit = transitionModel
                        .GetTransitionWithMaximumExpectedUtility(s, U);
                var maxPolicyTransit = transitionModel
                        .GetTransitionWithMaximumExpectedUtilityUsingPolicy(pi,
                                s, U);

                if (maxTransit.GetSecond() > maxPolicyTransit.GetSecond()) {
                    pi.SetAction(s, maxTransit.GetFirst());
                    unchanged = false;
                }
            }
        } while (unchanged == false);
        return pi;
    }

    public MDPUtilityFunction<TState> PolicyEvaluation(
            MDPPolicy<TState, TAction> pi,
            MDPUtilityFunction<TState> u, double gamma, int iterations) {
        var uDash = u.Copy();
        for (var i = 0; i < iterations; i++) {

            uDash = this.ValueIterateOnceWith(gamma, pi, uDash);
        }
        return uDash;
    }

    public MDPPolicy<TState, TAction> RandomPolicy() {
        var policy = new MDPPolicy<TState, TAction>();
        foreach (var s in nonFinalstates) {
            policy.SetAction(s, transitionModel.RandomActionFor(s));
        }
        return policy;
    }

    public MDPUtilityFunction<TState> InitialUtilityFunction() {

        return rewardFunction.AsUtilityFunction();
    }

    public TState GetInitialState() {
        return initialState;
    }

    public double GetRewardFor(TState state) {
        return rewardFunction.GetRewardFor(state);
    }

    public void SetReward(TState state, double reward) {
        rewardFunction.SetReward(state, reward);
    }

    public void SetTransitionProbability(
            MDPTransition<TState, TAction> transition,
            double probability) {
        transitionModel.SetTransitionProbability(transition.GetInitialState(),
                transition.GetAction(), transition.GetDestinationState(),
                probability);
    }

    public double GetTransitionProbability(
            MDPTransition<TState, TAction> transition) {
        return transitionModel.GetTransitionProbability(transition
                .GetInitialState(), transition.GetAction(), transition
                .GetDestinationState());
    }

    public MDPPerception<TState> Execute(TState state,
            TAction action, IRandomizer r) {
        return source.Execute(state, action, r);
    }

    public bool IsTerminalState(TState state) {
        return terminalStates.Contains(state);
    }

    public IList<MDPTransition<TState, TAction>> GetTransitionsWith(
            TState initial, TAction action) {
        return transitionModel.GetTransitionsWithStartingStateAndAction(
                initial, action);
    }

    public IList<TAction> GetAllActions() {
        return source.GetAllActions();
    }

    public override string ToString()
    {
        return String.Format(
            "initial State = {0}\n rewardFunction = {1}\n transitionModel = {2}\n states = {3}",
            initialState, rewardFunction, transitionModel, nonFinalstates);
    }

    private double ValueIterateOnceForGivenState(double gamma,
            MDPUtilityFunction<TState> presentUtilityFunction,
            TState state) {
        var highestUtilityTransition = transitionModel
                .GetTransitionWithMaximumExpectedUtility(state,
                        presentUtilityFunction);
        var utility = rewardFunction.GetRewardFor(state)
                + (gamma * highestUtilityTransition.GetSecond());

        return utility;
    }

    private MDPUtilityFunction<TState> ValueIterateOnceWith(double gamma,
            MDPPolicy<TState, TAction> pi,
            MDPUtilityFunction<TState> U) {
        MDPUtilityFunction<TState> uDash = U.Copy();
        
        foreach (var s in this.nonFinalstates) 
        {
            var highestPolicyTransition =
                this.transitionModel.GetTransitionWithMaximumExpectedUtilityUsingPolicy(pi, s, U);
            double utility = rewardFunction.GetRewardFor(s)
                   + (gamma * highestPolicyTransition.GetSecond());
            uDash.SetUtility(s, utility);

        }
        // TODO: debugging code
        // System.out.println("ValueIterationOnce before " + U);
        // System.out.println("ValueIterationOnce after " + U_dash);
        return uDash;
    }
}

}
