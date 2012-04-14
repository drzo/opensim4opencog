using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.NQueens
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Provides useful functions for two versions of the n-queens problem. The
    /// incremental formulation and the complete-state formulation share the same
    /// RESULT function but use different ACTIONS functions.
    /// </summary>
    public class NQueensFunctionFactory 
    {
        private static IActionsFunction _iActionsFunction;
        private static IActionsFunction _cActionsFunction;
        private static IResultFunction _resultFunction;

        /// <summary>
        /// Returns an ACTIONS function for the incremental formulation of the
        /// n-queens problem.
        /// </summary>
        /// <returns></returns>
        public static IActionsFunction GetIActionsFunction() 
        {
            if (null == _iActionsFunction) 
            {
                _iActionsFunction = new NQIActionsFunction();
            }
            return _iActionsFunction;
        }

        /// <summary>
        /// Returns an ACTIONS function for the complete-state formulation of the
        /// n-queens problem.
        /// </summary>
        /// <returns></returns>
        public static IActionsFunction GetCActionsFunction() 
        {
            if (null == _cActionsFunction) 
            {
                _cActionsFunction = new NQCActionsFunction();
            }
            return _cActionsFunction;
        }

        /// <summary>
        /// Returns a RESULT function for the n-queens problem.
        /// </summary>
        /// <returns></returns>
        public static IResultFunction GetResultFunction() 
        {
            if (_resultFunction == null) 
            {
                _resultFunction = new NQResultFunction();
            }
            return _resultFunction;
        }

        /// <summary>
        /// Assumes that queens are placed column by column, starting with an empty
        /// board, and provides queen placing actions for all non-attacked positions
        /// of the first free column.
        /// </summary>
        private class NQIActionsFunction : IActionsFunction 
        {
            public ISet<IAction> Actions(object state) 
            {
                NQueensBoard board = (NQueensBoard) state;

                ISet<IAction> actions = new HashedSet<IAction>();

                var numQueens = board.GetNumberOfQueensOnBoard();
                var boardSize = board.Size;
                for (var i = 0; i < boardSize; i++) 
                {
                    var newLocation = new XYLocation(numQueens, i);
                    if (!(board.IsSquareUnderAttack(newLocation))) 
                    {
                        actions.Add(new QueenAction(QueenAction.PlaceQueen,
                                newLocation));
                    }
                }

                return actions;
            }
        }

        /// <summary>
        /// Assumes exactly one queen in each column and provides all possible queen
        /// movements in vertical direction as actions.
        /// </summary>
        private class NQCActionsFunction : IActionsFunction 
        {

            public ISet<IAction> Actions(object state)
            {
                ISet<IAction> actions = new HashedSet<IAction>();
                var board = (NQueensBoard) state;
                for (var i = 0; i < board.Size; i++)
                    for (var j = 0; j < board.Size; j++) {
                        var loc = new XYLocation(i, j);
                        if (!board.QueenExistsAt(loc))
                            actions.Add(new QueenAction(QueenAction.MoveQueen,loc));
                    }
                return actions;
            }
        }

        /// <summary>
        /// Supports queen placing, queen removal, and queen movement actions.
        /// </summary>
        private class NQResultFunction : IResultFunction 
        {
            public object Result(object s, IAction a) 
            {
                if (a is QueenAction) {
                    QueenAction qa = (QueenAction) a;
                    NQueensBoard board = (NQueensBoard) s;
                    NQueensBoard newBoard = new NQueensBoard(board.Size);
                    newBoard.SetBoard(board.GetQueenPositions());
                    if (qa.GetName() == QueenAction.PlaceQueen)
                        newBoard.AddQueenAt(qa.GetLocation());
                    else if (qa.GetName() == QueenAction.RemoveQueen)
                        newBoard.RemoveQueenFrom(qa.GetLocation());
                    else if (qa.GetName() == QueenAction.MoveQueen)
                        newBoard.MoveQueenTo(qa.GetLocation());
                    s = newBoard;
                }
                // if action is not understood or is a NoOp
                // the result will be the current state.
                return s;
            }
        }
    }
}
