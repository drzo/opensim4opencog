using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    public class RabbitEyeDataSet : NNDataSet 
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

            TargetColumnNumbers.Add(1); // using zero based indexing
        }
    }
}
