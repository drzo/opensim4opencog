using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.EightPuzzle
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    public class EightPuzzleFunctionFactory 
    {
        private static IActionsFunction actionsFunction = null;
        private static IResultFunction resultFunction = null;

        // TODO: Should do something to make these two methods below thread safe
        public static IActionsFunction GetActionsFunction()
        {
            if (null == actionsFunction)
            {
                actionsFunction = new EpActionsFunction();
            }
            return actionsFunction;
        }

        public static IResultFunction getResultFunction()
        {
            if (null == resultFunction)
            {
                resultFunction = new EPResultFunction();
            }
            return resultFunction;
        }

        private class EpActionsFunction : IActionsFunction 
        {
            public ISet<IAction> Actions(object state) 
            {
                EightPuzzleBoard board = (EightPuzzleBoard) state;

                var actions = new HashedSet<IAction>();

                if (board.CanMoveGap(EightPuzzleBoard.Up)) {
                    actions.Add(EightPuzzleBoard.Up);
                }
                if (board.CanMoveGap(EightPuzzleBoard.Down)) {
                    actions.Add(EightPuzzleBoard.Down);
                }
                if (board.CanMoveGap(EightPuzzleBoard.Left))
                {
                    actions.Add(EightPuzzleBoard.Left);
                }
                if (board.CanMoveGap(EightPuzzleBoard.Right))
                {
                    actions.Add(EightPuzzleBoard.Right);
                }

                return actions;
            }
        }

        private class EPResultFunction : IResultFunction 
        {
            public object Result(object s, IAction a) 
            {
                EightPuzzleBoard board = (EightPuzzleBoard) s;

                if (EightPuzzleBoard.Up.Equals(a)
                        && board.CanMoveGap(EightPuzzleBoard.Up))
                {
                    EightPuzzleBoard newBoard = new EightPuzzleBoard(board);
                    newBoard.MoveGapUp();
                    return newBoard;
                }
                else if (EightPuzzleBoard.Down.Equals(a)
                        && board.CanMoveGap(EightPuzzleBoard.Down))
                {
                    EightPuzzleBoard newBoard = new EightPuzzleBoard(board);
                    newBoard.MoveGapDown();
                    return newBoard;
                }
                else if (EightPuzzleBoard.Left.Equals(a)
                        && board.CanMoveGap(EightPuzzleBoard.Left))
                {
                    EightPuzzleBoard newBoard = new EightPuzzleBoard(board);
                    newBoard.MoveGapLeft();
                    return newBoard;
                }
                else if (EightPuzzleBoard.Right.Equals(a)
                        && board.CanMoveGap(EightPuzzleBoard.Right))
                {
                    EightPuzzleBoard newBoard = new EightPuzzleBoard(board);
                    newBoard.MoveGapRight();
                    return newBoard;
                }

                // The Action is not understood or is a NoOp
                // the result will be the current state.
                return s;
            }
        }
    }
}
