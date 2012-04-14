using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Neural
{
    /// <summary>
    /// a holder for config data for neural networks and possibly for other
    /// learning systems.
    /// </summary>
    public class NNConfig {
        private readonly Dictionary<string, object> hash;

        public NNConfig(Dictionary<string, object> hash)
        {
            this.hash = hash;
        }

        public NNConfig() {
            this.hash = new Dictionary<string, object>();
        }

        public double GetParameterAsDouble(string key) {

            return (double) hash[key];
        }

        public int GetParameterAsInteger(string key) {

            return (int) hash[key];
        }

        public void SetConfig(string key, double value) {
            hash[key] = value;
        }

        public void SetConfig(string key, int value) {
            hash[key] = value;
        }
    }

}
