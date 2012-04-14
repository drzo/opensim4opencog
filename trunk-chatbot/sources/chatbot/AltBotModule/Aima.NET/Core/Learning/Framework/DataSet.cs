using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Framework
{
    using System.Collections;

    public class DataSet: IEnumerable<Example> {
        protected DataSet() {

        }

        //TODO:encapsulate these fields
        public IList<Example> examples;

        public DataSetSpecification Specification { get; private set; }

        public DataSet(DataSetSpecification spec) {
            //TODO: was originally LinkedList in java. Make sure that List is ok.
            examples = new List<Example>();
            this.Specification = spec;
        }

        public void Add(Example e) 
        {
            examples.Add(e);
        }

        public int Count
        {
            get
            {
                return examples.Count;   
            }
        }

        public Example GetExample(int number) 
        {
            return examples[number];
        }

        public DataSet RemoveExample(Example e) 
        {
            var ds = new DataSet(this.Specification);
            foreach (var eg in this.examples.Where(eg => !(e.Equals(eg))))
            {
                ds.Add(eg);
            }
            return ds;
        }

        public double GetInformationFor() 
        {
            var attributeName = Specification.TargetAttribute;
            var counts = new Dictionary<string, int>();
            foreach (var val in this.examples.Select(e => e.GetAttributeValueAsString(attributeName)))
            {
                if (counts.ContainsKey(val)) 
                {
                    counts[val] = counts[val] + 1;
                } 
                else 
                {
                    counts[val] = 1;
                }
            }

            var data = counts.Values.Cast<double>().ToArray();

            data = Util.Util.Normalize(data);

            return Util.Util.Information(data);
        }

        public Dictionary<string, DataSet> SplitByAttribute(string attributeName) 
        {
            var results = new Dictionary<string, DataSet>();
            foreach (Example e in examples) 
            {
                var val = e.GetAttributeValueAsString(attributeName);
                if (results.ContainsKey(val)) 
                {
                    results[val].Add(e);
                } 
                else 
                {
                    var ds = new DataSet(Specification);
                    ds.Add(e);
                    results[val] = ds;
                }
            }
            return results;
        }

        public double CalculateGainFor(string parameterName) 
        {
            var hash = this.SplitByAttribute(parameterName);
            double totalSize = examples.Count;
            var remainder = (from parameterValue in hash.Keys
                             let reducedDataSetSize = hash[parameterValue].examples.Count
                             select (reducedDataSetSize / totalSize) * hash[parameterValue].GetInformationFor()).Sum();
            return this.GetInformationFor() - remainder;
        }

        public IEnumerator<Example> GetEnumerator()
        {
            return examples.GetEnumerator();
        }

        public override bool Equals(object o) {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            return o.GetType() == typeof(DataSet) && this.Equals((DataSet)o);
        }

        public DataSet Copy() 
        {
            var ds = new DataSet(Specification);
            foreach (Example e in examples) 
            {
                ds.Add(e);
            }
            return ds;
        }

        public IList<string> GetAttributeNames() 
        {
            return Specification.GetAttributeNames();
        }

        public String GetTargetAttributeName() {
            return Specification.TargetAttribute;
        }

        public DataSet EmptyDataSet() 
        {
            return new DataSet(Specification);
        }

        public IList<string> GetPossibleAttributeValues(string attributeName) 
        {
            return Specification.GetPossibleAttributeValues(attributeName);
        }

        public DataSet MatchingDataSet(string attributeName, string attributeValue) 
        {
            DataSet ds = new DataSet(Specification);
            foreach (Example e in
                this.examples.Where(e => e.GetAttributeValueAsString(attributeName).Equals(attributeValue)))
            {
                ds.Add(e);
            }
            return ds;
        }

        public IList<String> GetNonTargetAttributes() {
            return Util.Util.RemoveFrom(this.GetAttributeNames(), this.GetTargetAttributeName());
        }

        public bool Equals(DataSet other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.examples, this.examples) && Equals(other.Specification, this.Specification);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.examples != null ? this.examples.GetHashCode() : 0) * 397) ^ (this.Specification != null ? this.Specification.GetHashCode() : 0);
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }

}
