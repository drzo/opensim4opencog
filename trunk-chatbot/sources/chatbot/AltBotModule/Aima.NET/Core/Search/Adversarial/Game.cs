using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Adversarial
{
    using System.Collections;

    public abstract class Game
    {
        protected GameState InitialState = new GameState();

        protected GameState PresentState = new GameState();

        protected int Level;

        public abstract List<GameState> GetSuccessorStates(GameState state);

        public abstract GameState MakeMove(GameState state, object o);

        public abstract int GetMiniMaxValue(GameState state);

        public abstract int GetAlphaBetaValue(GameState state);

        public bool HasEnded()
        {
            return (this.TerminalTest(this.GetState()));
        }

        public int GetLevel(GameState g)
        {
            return (Int32.Parse(g["level"].ToString()));
        }

        public ArrayList GetMoves(GameState state)
        {
            return (ArrayList)state["moves"];
        }

        public string GetPlayerToMove(GameState state)
        {
            return (String)state["player"];
        }

        public int GetUtility(GameState h)
        {
            return (Int32.Parse(h["utility"].ToString()));
        }

        public GameState GetState()
        {
            return this.PresentState;
        }

        public int MaxValue(GameState state)
        {
            var v = Int32.MinValue;
            
            if (this.TerminalTest(state))
            {
                return this.ComputeUtility(state);
            }
            
            var successorList = this.GetSuccessorStates(state);
            for (var i = 0; i < successorList.Count; i++)
            {
                var successor = successorList[i];
                var minimumValueOfSuccessor = this.MinValue(successor);
                if (minimumValueOfSuccessor > v)
                {
                    v = minimumValueOfSuccessor;
                    state["next"] = successor;
                }
            }
            return v;
        }

        public int MinValue(GameState state)
        {

            int v = Int32.MaxValue;

            if (this.TerminalTest(state))
            {
                return this.ComputeUtility(state);

            }
            
            var successorList = this.GetSuccessorStates(state);
            for (var i = 0; i < successorList.Count; i++)
            {
                var successor = successorList[i];
                var maximumValueOfSuccessors = this.MaxValue(successor);
                if (maximumValueOfSuccessors < v)
                {
                    v = maximumValueOfSuccessors;
                    state["next"] = successor;
                }
            }
            return v;
        }

        public int MinValue(GameState state, AlphaBeta ab)
        {
            var v = Int32.MaxValue;

            if (this.TerminalTest(state))
            {
                return (this.ComputeUtility(state));

            }
            else
            {
                var successorList = this.GetSuccessorStates(state);
                foreach (var successor in successorList)
                {
                    var maximumValueOfSuccessor = this.MaxValue(successor, ab.Copy());
                    if (maximumValueOfSuccessor < v)
                    {
                        v = maximumValueOfSuccessor;
                        state["next"] = successor;
                    }
                    if (v <= ab.Alpha)
                    {
                        // System.out.println("pruning from min");
                        return v;
                    }
                    ab.Beta = Util.Util.Min(ab.Beta, v);
                }
                return v;
            }

        }

        public void MakeMiniMaxMove()
        {
            this.GetMiniMaxValue(this.PresentState);
            var nextState = (GameState)this.PresentState["next"];
            if (nextState == null)
            {
                throw new ApplicationException("Mini Max Move failed");

            }
            this.MakeMove(this.PresentState, nextState["moveMade"]);

        }

        public void MakeAlphaBetaMove()
        {
            this.GetAlphaBetaValue(this.PresentState);

            GameState nextState = (GameState)this.PresentState["next"];
            if (nextState == null)
            {
                throw new ApplicationException("Alpha Beta Move failed");
            }
            this.MakeMove(this.PresentState, nextState["moveMade"]);

        }

        protected abstract int ComputeUtility(GameState state);

        protected abstract bool TerminalTest(GameState state);

        protected int MaxValue(GameState state, AlphaBeta ab)
        {
            int v = Int32.MinValue;
            if (this.TerminalTest(state))
            {
                return this.ComputeUtility(state);
            }
            else
            {
                var successorList = this.GetSuccessorStates(state);
                for (var i = 0; i < successorList.Count; i++)
                {
                    var successor = (GameState)successorList[i];
                    var minimumValueOfSuccessor = this.MinValue(successor, ab.Copy());
                    if (minimumValueOfSuccessor > v)
                    {
                        v = minimumValueOfSuccessor;
                        state["next"] = successor;
                    }
                    if (v >= ab.Beta)
                    {
                        // System.out.println("pruning from max");
                        return v;
                    }
                    ab.Alpha = Util.Util.Max(ab.Alpha, v);
                }
                return v;
            }
        }
    }
}
