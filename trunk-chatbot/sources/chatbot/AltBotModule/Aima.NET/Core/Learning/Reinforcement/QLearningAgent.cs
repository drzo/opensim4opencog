using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Reinforcement
{
    using Aima.Core.Probability.Decision;
    using Aima.Core.Util;
    using Aima.Core.Util.Datastructure;

    public class QLearningAgent<TState, TAction> : MDPAgent<TState, TAction> 
        where TState : class
        where TAction : class
    {

        private Dictionary<Pair<TState, TAction>, double> Q;

        private FrequencyCounter<Pair<TState, TAction>> stateActionCount;

        private double previousReward;

        private QTable<TState, TAction> qTable;

        private int actionCounter;

        public QLearningAgent(MDP<TState, TAction> mdp): base(mdp) 
        {
            Q = new Dictionary<Pair<TState, TAction>, double>();
            qTable = new QTable<TState, TAction>(mdp.GetAllActions());
            stateActionCount = new FrequencyCounter<Pair<TState, TAction>>();
            actionCounter = 0;
        }

        public override TAction DecideAction(MDPPerception<TState> perception) 
        {
            CurrentState = perception.GetState();
            CurrentReward = perception.GetReward();

            if (this.StartingTrial()) {
                TAction chosenAction = this.SelectRandomAction();
                this.UpdateLearnerState(chosenAction);
                return PreviousAction;
            }

            if (MDP.IsTerminalState(CurrentState)) {
                this.IncrementStateActionCount(PreviousState, PreviousAction);
                this.UpdateQ(0.8);
                PreviousAction = null;
                PreviousState = null;
                //TODO: make sure that this is ok value for what used to be null in java
                previousReward = double.NegativeInfinity;
                return PreviousAction;
            }

            else {
                this.IncrementStateActionCount(PreviousState, PreviousAction);
                TAction chosenAction = this.UpdateQ(0.8);
                this.UpdateLearnerState(chosenAction);
                return PreviousAction;
            }

        }

        public Dictionary<Pair<TState, TAction>, double> GetQ() {
            return Q;
        }

        public QTable<TState, TAction> GetQTable() {
            return qTable;
        }

        private void UpdateLearnerState(TAction chosenAction) {
            // PreviousAction = actionMaximizingLearningFunction();
            PreviousAction = chosenAction;
            PreviousAction = chosenAction;
            PreviousState = CurrentState;
            previousReward = CurrentReward;
        }

        //TODO: figure out wheter parameter not used is an issue or not
        private TAction UpdateQ(double gamma) {

            actionCounter++;
            // qtable update

            double alpha = this.CalculateProbabilityOf(PreviousState, PreviousAction);
            TAction ac = qTable.UpDateQ(PreviousState, PreviousAction,
                    CurrentState, alpha, CurrentReward, 0.8);

            return ac;
        }

        private double CalculateProbabilityOf(TState state, TAction action) {
            double den = 0.0;
            double num = 0.0;
            foreach (Pair<TState, TAction> stateActionPair in stateActionCount.GetStates()) 
            {
                if (stateActionPair.GetFirst().Equals(state)) {
                    den += 1;
                    if (stateActionPair.GetSecond().Equals(action)) {
                        num += 1;
                    }
                }
            }
            return num / den;
        }

        private TAction ActionMaximizingLearningFunction() {
            TAction maxAct = null;
            double maxValue = double.NegativeInfinity;
            foreach (TAction action in MDP.GetAllActions())
            {
                double qValue = qTable.GetQValue(CurrentState, action);
                var lfv = this.LearningFunction(qValue);
                if (lfv > maxValue)
                {
                    maxValue = lfv;
                    maxAct = action;
                }
            }
            return maxAct;
        }

        private double LearningFunction(double utility) {
            if (actionCounter > 3) {
                actionCounter = 0;
                return 1.0;
            } else {
                return utility;
            }
        }

        private TAction SelectRandomAction() {
            IList<TAction> allActions = MDP.GetAllActions();
            return allActions[0];
            // return Util.selectRandomlyFromList(allActions);
        }

        private bool StartingTrial() {
            return (PreviousAction == null) && (PreviousState == null)
                    && (previousReward == double.NegativeInfinity)
                    && (CurrentState.Equals(MDP.GetInitialState()));
        }

        private void IncrementStateActionCount(TState state, TAction action) {
            Pair<TState, TAction> stateActionPair = new Pair<TState, TAction>(
                    state, action);
            stateActionCount.IncrementFor(stateActionPair);
        }
    }

}
