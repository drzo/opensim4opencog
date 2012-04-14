using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.EightPuzzle
{
    using Aima.Core.Search.Framework;
    using Aima.Core.Util.Datastructure;

    public class ManhattanHeuristicFunction : IHeuristicFunction 
    {

        public double H(object state)
        {
            var board = (EightPuzzleBoard)state;
            var retVal = 0;
            for (int i = 1; i < 9; i++)
            {
                var loc = board.GetLocationOf(i);
                retVal += this.EvaluateManhattanDistanceOf(i, loc);
            }
            return retVal;
        }

        public int EvaluateManhattanDistanceOf(int i, XYLocation loc) 
        {
            var retVal = -1;
            var xpos = loc.XCoordinate;
            var ypos = loc.YCoordinate;
            switch (i)
            {
                case 1:
                retVal = Math.Abs(xpos - 0) + Math.Abs(ypos - 1);
                break;
            case 2:
                retVal = Math.Abs(xpos - 0) + Math.Abs(ypos - 2);
                break;
            case 3:
                retVal = Math.Abs(xpos - 1) + Math.Abs(ypos - 0);
                break;
            case 4:
                retVal = Math.Abs(xpos - 1) + Math.Abs(ypos - 1);
                break;
            case 5:
                retVal = Math.Abs(xpos - 1) + Math.Abs(ypos - 2);
                break;
            case 6:
                retVal = Math.Abs(xpos - 2) + Math.Abs(ypos - 0);
                break;
            case 7:
                retVal = Math.Abs(xpos - 2) + Math.Abs(ypos - 1);
                break;
            case 8:
                retVal = Math.Abs(xpos - 2) + Math.Abs(ypos - 2);
                break;
            }

            return retVal;
        }
    }
}
