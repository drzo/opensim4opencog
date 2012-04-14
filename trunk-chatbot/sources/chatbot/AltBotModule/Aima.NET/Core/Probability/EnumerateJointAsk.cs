using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    public class EnumerateJointAsk
    {

        public static double[] ask(Query q, ProbabilityDistribution pd)
        {
            double[] probDist = new double[2];
            Dictionary<string, bool?> h = q.EvidenceVariables;

            // true probability
            h[q.QueryVariable] = true;
            probDist[0] = pd.ProbabilityOf(h);
            // false probability
            h[q.QueryVariable] =false;
            probDist[1] = pd.ProbabilityOf(h);
            return Util.Util.Normalize(probDist);
        }
    }
}
