using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.NQueens
{
    using Aima.Core.Search.Framework;

    public class NQueensGoalTest : IGoalTest 
    {

        public bool IsGoalState(object state)
        {
            var board = (NQueensBoard)state;
            return board.GetNumberOfQueensOnBoard() == board.Size && board.GetNumberOfAttackingPairs() == 0;
        }
    }
}
