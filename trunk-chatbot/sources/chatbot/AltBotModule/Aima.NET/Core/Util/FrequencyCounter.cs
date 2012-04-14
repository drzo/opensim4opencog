using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util
{
    public class FrequencyCounter<T> 
    {
        private IDictionary<T, int> counter;

        public FrequencyCounter() {
            counter = new Dictionary<T, int>();
        }

        public int GetCount(T key) {
            int value = counter[key];
            return value;
        }

        public void IncrementFor(T key)
        {
            int value = counter[key];
            counter[key] = value + 1;
            
        }

        public double ProbabilityOf(T key) {
            int value = this.GetCount(key);
            if (value == 0) {
                return 0.0;
            }
            double total = counter.Keys.Aggregate(0.0, (current, k) => current + this.GetCount(k));
            return value / total;
        }

        public override string ToString() {
            return counter.ToString();
        }

        public ICollection<T> GetStates() //used to be ISet
        {
            return counter.Keys;
        }
    }
}
