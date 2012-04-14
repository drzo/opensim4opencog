using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    /// <summary>
    /// </summary>
    /// <typeparam name="TState">
    /// </typeparam>
    /// <typeparam name="TAction">
    /// </typeparam>
    public interface IMDPSource<TState, TAction>
        where TAction : class
    {
        MDP<TState, TAction> AsMdp();

        TState GetInitialState();

        MDPTransitionModel<TState, TAction> GetTransitionModel();

        MDPRewardFunction<TState> GetRewardFunction();

        IList<TState> GetNonFinalStates();

        IList<TState> GetFinalStates();

        MDPPerception<TState> Execute(TState state, TAction action,
                IRandomizer r);

        IList<TAction> GetAllActions();
    }
}
