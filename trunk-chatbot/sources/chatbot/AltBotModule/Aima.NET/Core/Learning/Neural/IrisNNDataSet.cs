using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public class IrisNNDataSet : NNDataSet 
    {

        public override void SetTargetColumns() 
        {
            // assumed that data from file has been pre processed
            // TODO this should be
            // somewhere else,in the
            // super class.
            // Type != class Aargh! I want more
            // powerful type systems
            TargetColumnNumbers = new List<int>();
            int size = Nds[0].Count;
            TargetColumnNumbers.Add(size - 1); // last column
            TargetColumnNumbers.Add(size - 2); // last but one column
            TargetColumnNumbers.Add(size - 3); // and the one before that
        }
    }
}
