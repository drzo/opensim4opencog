using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.CellWorld
{
    public class Cell
    {
        public int X { get; set; }
        public int Y { get; set; }

        public double Reward { get; set; }

        public Cell(int i, int j, double reward)
        {
            this.X = i;
            this.Y = j;
            this.Reward = reward;
        }

        public CellWorldPosition Position()
        {
            return new CellWorldPosition(this.X, this.Y);
        }
    }

}
