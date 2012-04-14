using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.EightPuzzle
{
    using Aima.Core.Search.Framework;

    public class EightPuzzleGoalTest : IGoalTest 
    {
        private EightPuzzleBoard goal = new EightPuzzleBoard(new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8 });

        public bool IsGoalState(object state)
        {
            var board = (EightPuzzleBoard)state;
            return board.Equals(goal);
        }
    }
}
