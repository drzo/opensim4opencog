using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;

    public class MajorityLearner : ILearner 
    {

        private string result;

        public void Train(DataSet ds) 
        {
            IList<string> targets = new List<string>();
            foreach (Example e in ds.examples) 
            {
                targets.Add(e.TargetValue());
            }
            result = Util.Util.Mode(targets);
        }

        public string Predict(Example e) {
            return result;
        }

        public int[] Test(DataSet ds) {
            int[] results = new int[] { 0, 0 };

            foreach (Example e in ds.examples) {
                if (e.TargetValue().Equals(result)) {
                    results[0] = results[0] + 1;
                } else {
                    results[1] = results[1] + 1;
                }
            }
            return results;
        }
    }
}
