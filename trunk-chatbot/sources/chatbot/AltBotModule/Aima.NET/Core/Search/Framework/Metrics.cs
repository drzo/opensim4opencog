using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Search.Framework
{
    public class Metrics
    {
        //TODO: there must be better way to store int/double values in common type than string 
        private Dictionary<string, string> hash;

        public Metrics()
        {
            this.hash = new Dictionary<string, string>();
        }

        public void Set(string name, int i)
        {
            this.hash[name] = i.ToString();
        }

        public void Set(string name, double d)
        {
            hash[name] = d.ToString();
        }

        public int GetInt(string name)
        {
            return Int32.Parse(hash[name]);
        }

        public double GetDouble(string name)
        {
            return double.Parse(hash[name]);
        }

        public string Get(string name)
        {
            return hash[name];
        }

        // TODO: Decide about proper implementation of methods returning ISets especially those returning dictionary keys
        public ISet<string> KeySet()
        {
            var keys = new HashedSet<string>();
            keys.UnionWith(hash.Keys);
            return keys;
        }
    }
}
