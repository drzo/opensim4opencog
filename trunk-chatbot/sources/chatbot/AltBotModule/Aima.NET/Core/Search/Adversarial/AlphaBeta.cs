using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Adversarial
{
    public class AlphaBeta
    {
        public int Alpha { get; set;}

        public int Beta { get; set; }

        public AlphaBeta(int alpha, int beta)
        {
            this.Alpha = alpha;
            this.Beta = beta;
        }

        public AlphaBeta Copy()
        {
            return new AlphaBeta(Alpha, Beta);
        }
    }

}
