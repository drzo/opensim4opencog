using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Inductive
{
    using Aima.Core.Learning.Framework;

    public class DecisionList 
    {
        private string positive, negative;

        private IList<DLTest> tests;

        private Dictionary<DLTest, string> testOutcomes;

        public DecisionList(string positive, string negative) 
        {
            this.positive = positive;
            this.negative = negative;
            this.tests = new List<DLTest>();
            testOutcomes = new Dictionary<DLTest, string>();
        }

        public String Predict(Example example) 
        {
            if (tests.Count == 0) 
            {
                return negative;
            }
            foreach (var test in this.tests.Where(test => test.Matches(example)))
            {
                return this.testOutcomes[test];
            }
            return negative;
        }

        public void Add(DLTest test, string outcome) 
        {
            tests.Add(test);
            testOutcomes[test] = outcome;
        }

        public DecisionList MergeWith(DecisionList dlist2) 
        {
            var merged = new DecisionList(positive, negative);
            foreach (var test in tests) {
                merged.Add(test, testOutcomes[test]);
            }
            foreach (var test in dlist2.tests) 
            {
                merged.Add(test, dlist2.testOutcomes[test]);
            }
            return merged;
        }

        public override string ToString() 
        {
            var buf = new StringBuilder();
            foreach (var test in this.tests)
            {
                buf.Append(test);
                buf.Append(" => ");
                buf.Append(this.testOutcomes[test]);
                buf.Append(" ELSE \n");
            }
            buf.Append("END");
            return buf.ToString();
        }
    }

}
