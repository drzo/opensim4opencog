using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.EightPuzzle
{
    using Aima.Core.Search.Framework;
    using Aima.Core.Util.Datastructure;

    public class MisplacedTilleHeuristicFunction : IHeuristicFunction 
    {

        public double H(object state) 
        {
            var board = (EightPuzzleBoard) state;
            return this.GetNumberOfMisplacedTiles(board);
        }

        private int GetNumberOfMisplacedTiles(EightPuzzleBoard board) 
        {
            var numberOfMisplacedTiles = 0;
            if (!(board.GetLocationOf(0).Equals(new XYLocation(0, 0)))) 
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(1).Equals(new XYLocation(0, 1))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(2).Equals(new XYLocation(0, 2))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(3).Equals(new XYLocation(1, 0))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(4).Equals(new XYLocation(1, 1))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(5).Equals(new XYLocation(1, 2))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(6).Equals(new XYLocation(2, 0))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(7).Equals(new XYLocation(2, 1))))
            {
                numberOfMisplacedTiles++;
            }
            if (!(board.GetLocationOf(8).Equals(new XYLocation(2, 2))))
            {
                numberOfMisplacedTiles++;
            }
            return numberOfMisplacedTiles;
        }
    }
}
