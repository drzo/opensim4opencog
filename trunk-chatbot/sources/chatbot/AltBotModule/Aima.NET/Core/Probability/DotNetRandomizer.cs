using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    class DotNetRandomizer : IRandomizer 
    {
        static Random r = new Random();

        public double NextDouble() {
            return r.NextDouble();
        }
    }
}
