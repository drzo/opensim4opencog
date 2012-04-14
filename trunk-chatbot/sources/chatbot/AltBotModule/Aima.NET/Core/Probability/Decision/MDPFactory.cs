using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    using Aima.Core.Environment.CellWorld;

    public class MDPFactory
    {

        public static MDP<CellWorldPosition, String> CreateFourByThreeMDP()
        {

            CellWorld cw = new CellWorld(3, 4, 0.4);
            cw = new CellWorld(3, 4, -0.04);

            cw.MarkBlocked(2, 2);

            cw.SetTerminalState(2, 4);
            cw.SetReward(2, 4, -1);

            cw.SetTerminalState(3, 4);
            cw.SetReward(3, 4, 1);
            return cw.AsMdp();
        }
    }

}
