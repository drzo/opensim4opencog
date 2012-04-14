using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Inductive
{
    using Aima.Core.Learning.Framework;

    public class DLTest 
    {

        // represents a single test in the Decision List
        private Dictionary<string, string> attrValues;

        public DLTest() 
        {
            attrValues = new Dictionary<string, string>();
        }

        public void Add(string nta, string ntaValue) 
        {
            attrValues[nta] = ntaValue;
        }

        public bool Matches(Example e) 
        {
            foreach (string key in attrValues.Keys) 
            {
                if (!(attrValues[key].Equals(e.GetAttributeValueAsString(key)))) 
                {
                    return false;
                }
            }
            return true;
            // return e.targetValue().equals(targetValue);
        }

        public DataSet MatchedExamples(DataSet ds) 
        {
            var matched = ds.EmptyDataSet();
            foreach (Example e in ds.examples.Where(e => this.Matches(e)))
            {
                matched.Add(e);
            }
            return matched;
        }

        public DataSet UnmatchedExamples(DataSet ds) 
        {
            var unmatched = ds.EmptyDataSet();
            foreach (var e in ds.examples.Where(e => !(this.Matches(e))))
            {
                unmatched.Add(e);
            }
            return unmatched;
        }

        public override string ToString() 
        {
            var buf = new StringBuilder();
            buf.Append("IF  ");
            foreach (string key in attrValues.Keys) 
            {
                buf.Append(key);
                buf.Append(" = ");
                buf.Append(attrValues[key]);
                buf.Append(" ");
            }
            buf.Append(" DECISION ");
            return buf.ToString();
        }
    }

}
