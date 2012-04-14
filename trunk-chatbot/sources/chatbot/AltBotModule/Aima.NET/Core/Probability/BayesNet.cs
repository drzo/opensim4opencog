using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    public class BayesNet 
    {
        private IList<BayesNetNode> roots = new List<BayesNetNode>();

        private IList<BayesNetNode> variableNodes;

        public BayesNet(BayesNetNode root) {
            roots.Add(root);
        }

        public BayesNet(BayesNetNode root1, BayesNetNode root2): this(root1) {
            roots.Add(root2);
        }

        public BayesNet(BayesNetNode root1, BayesNetNode root2, BayesNetNode root3) :this(root1, root2) {
            roots.Add(root3);
        }

        public BayesNet(IList<BayesNetNode> rootNodes) {
            roots = rootNodes;
        }

        public IList<string> GetVariables() {
            variableNodes = this.GetVariableNodes();
            return this.variableNodes.Select(variableNode => variableNode.Variable).ToList();
        }

        public double ProbabilityOf(string Y, bool? value,
                Dictionary<string, bool?> evidence) {
            BayesNetNode y = this.GetNodeOf(Y);
            if (y == null) {
                throw new ApplicationException("Unable to find a node with variable "
                        + Y);
            }

            IList<BayesNetNode> parentNodes = y.Parents;
            if (parentNodes.Count == 0) {// root nodes
                Dictionary<string, bool?> yTable = new Dictionary<string, bool?>();
                yTable[Y] = value;

                double prob = y.ProbabilityOf(yTable);
                return prob;

            } else {// non rootnodes
                Dictionary<string, bool?> parentValues = new Dictionary<string, bool?>();
                foreach (BayesNetNode parent in parentNodes) {
                    parentValues[parent.Variable] = evidence[parent.Variable];
                }
                double prob = y.ProbabilityOf(parentValues);
                if (value.Equals(true)) {
                    return prob;
                }
                return (1.0 - prob);
            }
                }

        public Dictionary<string, bool?> GetPriorSample(IRandomizer r) 
        {
            var h = new Dictionary<string, bool?>();
            var bayesNetNodes = this.GetVariableNodes();
            foreach (var node in bayesNetNodes) {
                h[node.Variable] = node.IsTrueFor(r.NextDouble(), h);
            }
            return h;
        }

        public Dictionary<string, bool?> GetPriorSample() {
            return this.GetPriorSample(new DotNetRandomizer());
        }

        public double[] RejectionSample(string X, Dictionary<string, bool> evidence,
                int numberOfSamples, IRandomizer r) {
            double[] retval = new double[2];
            for (int i = 0; i < numberOfSamples; i++) {
                var sample = this.GetPriorSample(r);
                if (this.Consistent(sample, evidence))
                {
                    bool? queryValue = sample[X];
                    if (queryValue == true) {
                        retval[0] += 1;
                    } else {
                        retval[1] += 1;
                    }
                }
            }
            return Util.Util.Normalize(retval);
        }

        public double[] LikelihoodWeighting(string X,
                Dictionary<string, bool?> evidence, int numberOfSamples,
                IRandomizer r) 
        {
            var retval = new double[2];
            for (int i = 0; i < numberOfSamples; i++) 
            {
                var x = new Dictionary<string, bool?>();
                var w = 1.0;
                var bayesNetNodes = this.GetVariableNodes();
                foreach (BayesNetNode node in bayesNetNodes) 
                {
                    if (evidence[node.Variable] != null) 
                    {
                        w *= node.ProbabilityOf(x);
                        x[node.Variable] = evidence[node.Variable];
                    } else {
                        x[node.Variable] = node.IsTrueFor(r.NextDouble(), x);
                    }
                }
                bool? queryValue = (x[X]);
                if (queryValue == true) {
                    retval[0] += w;
                } else {
                    retval[1] += w;
                }

            }
            return Util.Util.Normalize(retval);
        }

        public double[] McmcAsk(string X, Dictionary<string, bool> evidence, int numberOfVariables, IRandomizer r) 
        {
            double[] retval = new double[2];
            IList<string> nonEvidenceVariables = NonEvidenceVariables(evidence);
            Dictionary<string, bool> ev = this.CreateRandomEvent(nonEvidenceVariables, evidence, r);
            for (int j = 0; j < numberOfVariables; j++) 
            {
                foreach(var variable in nonEvidenceVariables)
                {
                    var node = this.GetNodeOf(variable);
                    var markovBlanket = this.MarkovBlanket(node);
                    var mb = this.CreateMBValues(markovBlanket, ev);
                    // event.put(node.Variable, node.IsTrueFor(
                    // r.getProbability(), mb));
                    ev[node.Variable] = this.TruthValue(this.RejectionSample(node.Variable, mb, 100, r), r);
                    bool queryValue = (ev[X]);
                    if (queryValue) {
                        retval[0] += 1;
                    } else {
                        retval[1] += 1;
                    }
                }
            }
            return Util.Util.Normalize(retval);
        }

        public double[] McmcAsk(string X, Dictionary<string, bool> evidence,
                int numberOfVariables) {
            return this.McmcAsk(X, evidence, numberOfVariables, new DotNetRandomizer());
        }

        public double[] LikelihoodWeighting(string X,
                Dictionary<string, bool?> evidence, int numberOfSamples) {
            return this.LikelihoodWeighting(X, evidence, numberOfSamples,
                    new DotNetRandomizer());
        }

        public double[] RejectionSample(string X,
                Dictionary<string, bool> evidence, int numberOfSamples) {
            return this.RejectionSample(X, evidence, numberOfSamples,
                    new DotNetRandomizer());
        }

        //
        // PRIVATE METHODS
        //

        private IList<BayesNetNode> GetVariableNodes() {
            // TODO dicey initalisation works fine but unclear . clarify
            if (variableNodes == null) {
                IList<BayesNetNode> newVariableNodes = new List<BayesNetNode>();
                IList<BayesNetNode> parents = roots;
                IList<BayesNetNode> traversedParents = new List<BayesNetNode>();

                while (parents.Count != 0) {
                    IList<BayesNetNode> newParents = new List<BayesNetNode>();
                    foreach (BayesNetNode parent in parents) {
                        // if parent unseen till now
                        if (!(traversedParents.Contains(parent))) {
                            newVariableNodes.Add(parent);
                            // Add any unseen children to next generation of parents
                            IList<BayesNetNode> children = parent.Children;
                            foreach (BayesNetNode child in children) {
                                if (!newParents.Contains(child)) {
                                    newParents.Add(child);
                                }
                            }
                            traversedParents.Add(parent);
                        }
                    }

                    parents = newParents;
                }
                variableNodes = newVariableNodes;
            }

            return variableNodes;
        }

        private BayesNetNode GetNodeOf(string y) {
            IList<BayesNetNode> bayesNetNodes = this.GetVariableNodes();
            foreach (BayesNetNode node in bayesNetNodes) {
                if (node.Variable.Equals(y)) {
                    return node;
                }
            }
            return null;
        }

        private bool Consistent(Dictionary<string, bool?> sample, Dictionary<string, bool> evidence) 
        {
            foreach(var key in evidence.Keys)
            {
                bool value = (bool) evidence[key];
                if (!(value.Equals(sample[key]))) {
                    return false;
                }
            }
            return true;
        }

        private bool TruthValue(double[] ds, IRandomizer r) 
        {
            double value = r.NextDouble();
            if (value < ds[0]) {
                return true;
            }
            return false;
        }

        private Dictionary<string, bool> CreateRandomEvent(IList<string> nonEvidenceVariables, Dictionary<string, bool> evidence, IRandomizer r) 
        {
            var table = new Dictionary<string, bool>();
            var variables = this.GetVariables();
            foreach (var variable in variables)
            {
                if (nonEvidenceVariables.Contains(variable)) 
                {
                    var value = r.NextDouble() <= 0.5 ? true : false;
                    table[variable] = value;
                }
                else
                {
                    table[variable] = evidence[variable];
                }
            }
            return table;
        }

        private IList<string> NonEvidenceVariables(Dictionary<string, bool> evidence) 
        {
            var variables = this.GetVariables();
            return variables.Where(variable => !(evidence.Keys.Contains(variable))).ToList();
        }

        private IList<BayesNetNode> MarkovBlanket(BayesNetNode node) 
        {
            return this.MarkovBlanket(node, new List<BayesNetNode>());
        }

        private IList<BayesNetNode> MarkovBlanket(BayesNetNode node,
                IList<BayesNetNode> soFar) {
            // parents
            IList<BayesNetNode> parents = node.Parents;
            foreach (BayesNetNode parent in parents) {
                if (!soFar.Contains(parent)) {
                    soFar.Add(parent);
                }
            }
            // children
            IList<BayesNetNode> children = node.Children;
            foreach (BayesNetNode child in children) {
                if (!soFar.Contains(child)) {
                    soFar.Add(child);
                    IList<BayesNetNode> childsParents = child.Parents;
                    foreach (BayesNetNode childsParent in childsParents) {
                        ;
                        if ((!soFar.Contains(childsParent))
                                && (!(childsParent.Equals(node)))) {
                            soFar.Add(childsParent);
                        }
                    }// childsParents
                }// end Contains child

            }// end child

            return soFar;
        }

        private Dictionary<string, bool> CreateMBValues(IList<BayesNetNode> markovBlanket,
                Dictionary<string, bool> ev)
        {
            var table = new Dictionary<string, bool>();
            foreach (BayesNetNode node in markovBlanket) 
            {
                table[node.Variable] = ev[node.Variable];
            }
            return table;
        }
    }
}
