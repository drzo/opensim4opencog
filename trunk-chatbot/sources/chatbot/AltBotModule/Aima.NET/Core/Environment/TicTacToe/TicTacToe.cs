using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.TicTacToe
{
    using System.Collections;
    using System.Diagnostics;

    using Aima.Core.Search.Adversarial;
    using Aima.Core.Util.Datastructure;

    public class TicTacToe : Game 
    {
        public TicTacToe() 
        {
            var moves = new List<XYLocation>();
            for (var i = 0; i < 3; i++) {
                for (var j = 0; j < 3; j++) {
                    var loc = new XYLocation(i, j);
                    moves.Add(loc);
                }
            }
            
            InitialState["moves"] = moves;
            InitialState["player"] = "X";
            InitialState["utility"] = 0;
            InitialState["board"] = new TicTacToeBoard();
            InitialState["level"] = 0;
            PresentState = InitialState;
        }

        public TicTacToeBoard GetBoard(GameState state) {

            return (TicTacToeBoard) state["board"];
        }

        public override List<GameState> GetSuccessorStates(GameState state) 
        {
            var temp = PresentState;
            var retVal = new List<GameState>();
            var parentLevel = GetLevel(state);
            for (var i = 0; i < GetMoves(state).Count; i++) {
                var loc = (XYLocation) GetMoves(state)[i];

                var aState = MakeMove(state, loc);
                aState["moveMade"] = loc;
                aState["level"] = parentLevel + 1;
                retVal.Add(aState);

            }
            PresentState = temp;
            return retVal;
        }

        public override GameState MakeMove(GameState state, object o) 
        {
            var loc = (XYLocation) o;
            return MakeMove(state, loc.XCoordinate, loc.YCoordinate);
        }

        public GameState MakeMove(GameState state, int x, int y) {
            var temp = this.GetMove(state, x, y);
            if (temp != null) {
                PresentState = temp;
            }
            return PresentState;
        }

        public GameState MakeMove(int x, int y) {
            var state = PresentState;
            var temp = this.GetMove(state, x, y);
            if (temp != null) {
                PresentState = temp;
            }
            return PresentState;
        }

        public GameState GetMove(GameState state, int x, int y) {
            GameState retVal = null;
            var loc = new XYLocation(x, y);
            var moves = GetMoves(state);
            var newMoves = (ArrayList) moves.Clone();
            if (moves.Contains(loc)) 
            {
                var index = newMoves.IndexOf(loc);
                newMoves.RemoveAt(index);

                retVal = new GameState();

                retVal["moves"] = newMoves;
                var newBoard = this.GetBoard(state).CloneBoard();
                if (GetPlayerToMove(state) == "X") 
                {
                    newBoard.MarkX(x, y);
                    retVal["player"] = "O";

                } 
                else 
                {
                    newBoard.MarkO(x, y);
                    retVal["player"] = "X";

                }
                retVal["board"] = newBoard;
                retVal["utility"] = ComputeUtility(newBoard, GetPlayerToMove(GetState()));
                retVal["level"] = GetLevel(state) + 1;
            }
            return retVal;
        }

        protected override int ComputeUtility(GameState state) 
        {
            int utility = ComputeUtility((TicTacToeBoard) state["board"],GetPlayerToMove(state));
            return utility;
        }

        protected override bool TerminalTest(GameState state) 
        {
            var board = (TicTacToeBoard) state["board"];
            var line = board.LineThroughBoard();
            var filled = board.GetNumberOfMarkedPositions() == 9;
            return (line || filled);
        }

        public void PrintPossibleMoves() {
            Debug.WriteLine("Possible moves");

            ArrayList moves = GetMoves(PresentState);
            foreach (var newState in
                from XYLocation moveLoc in moves
                select this.GetMove(this.PresentState, moveLoc.XCoordinate, moveLoc.YCoordinate))
            {
                Debug.WriteLine("utility = " + this.ComputeUtility(newState));
                Debug.WriteLine("");
            }

        }

        public override int GetMiniMaxValue(GameState state)
        {
            // statesSeen = new ArrayList();
            // System.out.println("In get Minimax Value");
            // System.out.println("Received state ");
            // ((TicTacToeBoard)state.get("board")).print();
            return String.Compare(this.GetPlayerToMove(state), "X", true) == 0
                       ? this.MaxValue(state)
                       : this.MinValue(state);
        }

        public override int GetAlphaBetaValue(GameState state) 
        {

            if (String.Compare(this.GetPlayerToMove(state), "X", true) == 0)
            {
                var initial = new AlphaBeta(Int32.MinValue,Int32.MaxValue);
                var max = MaxValue(state, initial);
                return max;

            } else {
                // invert?
                var initial = new AlphaBeta(Int32.MinValue,Int32.MaxValue);
                return MinValue(state, initial);
            }
        }

        private int ComputeUtility(TicTacToeBoard aBoard, string playerToMove) 
        {
            var retVal = 0;
            if (aBoard.LineThroughBoard()) {
                if (playerToMove.Equals("X")) {
                    retVal = -1;
                } else {
                    retVal = 1;
                }

            }
            return retVal;
        }
    }
}
