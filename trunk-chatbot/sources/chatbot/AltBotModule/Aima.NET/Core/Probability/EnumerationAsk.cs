using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    public class EnumerationAsk
    {

        public static double[] Ask(Query q, BayesNet net)
        {
            Dictionary<string, bool?> evidenceVariables = q.EvidenceVariables;

            double[] probDist = new double[2];
            // true probability
            evidenceVariables[q.QueryVariable] = true;
            probDist[0] = EnumerateAll(net, net.GetVariables(), evidenceVariables);
            // false probability
            evidenceVariables[q.QueryVariable] = false;
            probDist[1] = EnumerateAll(net, net.GetVariables(), evidenceVariables);
            // System.out.println( probDist[0] + " " + probDist[1]);
            // return probDist;
            double[] normalized = Util.Util.Normalize(probDist);
            // System.out.println( normalized[0] + " " + normalized[1]);
            return normalized;
        }

        private static double EnumerateAll(BayesNet net, IList<string> unprocessedVariables,
                Dictionary<string, bool?> evidenceVariables)
        {
            if (unprocessedVariables.Count == 0)
            {

                return 1.0;
            }

            var y = unprocessedVariables[0];

            if (evidenceVariables.Keys.Contains(y))
            {

                double probYGivenParents = net.ProbabilityOf(y, evidenceVariables[y], evidenceVariables);

                double secondTerm = EnumerateAll(net, Util.Util.Rest(unprocessedVariables), evidenceVariables);

                return probYGivenParents * secondTerm;
            }
            else
            {
                double sigma = 0.0;
                Dictionary<string, bool?> clone1 = CloneEvidenceVariables(evidenceVariables);
                clone1[y] = true;
                double probYTrueGivenParents = net.ProbabilityOf(y,
                    true, clone1);

                double secondTerm = EnumerateAll(net, Util.Util.Rest(unprocessedVariables), clone1);

                double trueProbabilityY = probYTrueGivenParents * secondTerm;

                Dictionary<string, bool?> clone2 = CloneEvidenceVariables(evidenceVariables);
                clone2[y] = false;
                double probYFalseGivenParents = net.ProbabilityOf(y,
                    false, clone2);

                secondTerm = EnumerateAll(net, Util.Util.Rest(unprocessedVariables),
                    clone2);
                double falseProbabilityY = probYFalseGivenParents * secondTerm;
                // System.out.print(secondTerm + " ) )");
                sigma = trueProbabilityY + falseProbabilityY;
                return sigma;

            }
        }

        private static Dictionary<string, bool?> CloneEvidenceVariables(
                Dictionary<string, bool?> evidence) 
        {
            var cloned = new Dictionary<string, bool?>();
            foreach (var key in evidence.Keys)
            {
                bool? b = evidence[key];
                if (b == true) 
                {
                    cloned[key] = true;
                } 
                else if ((evidence[key]).Equals(false)) 
                {
                    cloned[key] = false;
                }
            }
            return cloned;
        }
    }
}
