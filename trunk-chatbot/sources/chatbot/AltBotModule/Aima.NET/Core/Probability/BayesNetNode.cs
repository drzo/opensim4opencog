using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    using System.Collections;

    public class BayesNetNode 
    {
        public string Variable { get; private set; }

        public IList<BayesNetNode> Parents { get; private set; }
        
        public IList<BayesNetNode> Children { get; private set; }

        ProbabilityDistribution distribution;

        public BayesNetNode(string variable) {
            this.Variable = variable;
            this.Parents = new List<BayesNetNode>();
            this.Children = new List<BayesNetNode>();
            distribution = new ProbabilityDistribution(variable);
        }

        public void InfluencedBy(BayesNetNode parent1) {
            this.AddParent(parent1);
            parent1.AddChild(this);
            distribution = new ProbabilityDistribution(parent1.Variable);
        }

        public void InfluencedBy(BayesNetNode parent1, BayesNetNode parent2) {
            this.InfluencedBy(parent1);
            this.InfluencedBy(parent2);
            distribution = new ProbabilityDistribution(parent1.Variable,
                    parent2.Variable);
        }

        public void SetProbability(bool b, double d) {
            distribution.Set(b, d);
            if (this.IsRoot()) {
                distribution.Set(!b, 1.0 - d);
            }

        }

        public void SetProbability(bool b, bool c, double d) {
            distribution.Set(b, c, d);

        }

        public override string ToString() {
            return this.Variable;
        }

        public double ProbabilityOf(Dictionary<string, bool?> conditions) 
        {
            return distribution.ProbabilityOf(conditions);
        }

        public bool IsTrueFor(double probability,
                Dictionary<string, bool?> modelBuiltUpSoFar) 
        {
            Dictionary<string, bool?> conditions = new Dictionary<string, bool?>();
            if (this.IsRoot()) {
                conditions[Variable] = true;
            } else {
                for (int i = 0; i < this.Parents.Count; i++) {
                    BayesNetNode parent = this.Parents[i];
                    conditions[parent.Variable] = modelBuiltUpSoFar[parent.Variable];
                }
            }
            double trueProbability = this.ProbabilityOf(conditions);
            if (probability <= trueProbability) {
                return true;
            }
            return false;
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
            if (o.GetType() != typeof(BayesNetNode))
            {
                return false;
            }
            return Equals((BayesNetNode)o);
        }

        private void AddParent(BayesNetNode node) {
            if (!(this.Parents.Contains(node))) {
                this.Parents.Add(node);
            }
        }

        private void AddChild(BayesNetNode node) {
            if (!(this.Children.Contains(node))) {
                this.Children.Add(node);
            }
        }

        private bool IsRoot() {
            return (this.Parents.Count == 0);
        }

        public bool Equals(BayesNetNode other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Variable, this.Variable);
        }

        public override int GetHashCode()
        {
            return (this.Variable != null ? this.Variable.GetHashCode() : 0);
        }
    }
}
