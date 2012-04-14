using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.NQueens
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Estimates the distance to goal by the number of attacking pairs of queens on
    /// the board.
    /// </summary>
    public class AttackingPairsHeuristic : IHeuristicFunction {

        public double H(object state) {
            NQueensBoard board = (NQueensBoard) state;
            return board.GetNumberOfAttackingPairs();
        }
    }
}
