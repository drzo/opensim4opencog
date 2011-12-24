using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.PennBank.DiscourseBank
{
    /// <summary>
    /// Represents a PDTB relation
    /// </summary>
    public class DiscourseBankRelation
    {
        private string _relation;
        private string _semanticClass;
        private Dictionary<int, List<Span>> _arg1Spans;
        private Dictionary<int, List<Span>> _arg2Spans;

        /// <summary>
        /// Gets the relation
        /// </summary>
        public string Relation
        {
            get { return _relation; }
        }

        /// <summary>
        /// Gets the semantic class
        /// </summary>
        public string SemanticClass
        {
            get { return _semanticClass; }
        }

        /// <summary>
        /// Gets sentence spans for the Arg1
        /// </summary>
        public Dictionary<int, List<Span>> Arg1Spans
        {
            get { return _arg1Spans; }
        }

        /// <summary>
        /// Gets sentence spans for the Arg2
        /// </summary>
        public Dictionary<int, List<Span>> Arg2Spans
        {
            get { return _arg2Spans; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="relation">Relation type (e.g., explicit)</param>
        /// <param name="semanticClass">Semantic class</param>
        /// <param name="arg1Nodes">Nodes that comprise the Arg1</param>
        /// <param name="arg2Nodes">Nodes that comprise the Arg2</param>
        public DiscourseBankRelation(string relation, string semanticClass, List<TreeBankNode> arg1Nodes, List<TreeBankNode> arg2Nodes)
        {
            _relation = relation;
            _semanticClass = semanticClass;
            _arg1Spans = GetSpans(arg1Nodes);
            _arg2Spans = GetSpans(arg2Nodes);
        }

        /// <summary>
        /// Gets spans for a set of argument nodes, indexed by sentence
        /// </summary>
        /// <param name="argNodes">Argument nodes</param>
        /// <returns>Contiguous spans, indexed by sentence</returns>
        private Dictionary<int, List<Span>> GetSpans(List<TreeBankNode> argNodes)
        {
            // make sure all discourse annotation nodes come from the same source document
            List<TreeBankNode> allNodes = new List<TreeBankNode>();
            foreach (TreeBankNode node in argNodes)
                if (node.MrgFile != argNodes[0].MrgFile)
                    throw new Exception("MRG file mismatch");
                else
                    allNodes.Add(node);

            // remove any null nodes and sort the result by node position
            for (int i = 0; i < allNodes.Count; )
                if (allNodes[i].IsNullElement)
                    allNodes.RemoveAt(i);
                else
                    ++i;

            if (allNodes.Count == 0)
                throw new Exception("Invalid node list");

            // group nodes by sentence
            Dictionary<int, List<TreeBankNode>> sentenceNodes = new Dictionary<int, List<TreeBankNode>>();
            foreach (TreeBankNode node in allNodes)
            {
                sentenceNodes.EnsureContainsKey(node.SentenceNumber, typeof(List<TreeBankNode>));
                sentenceNodes[node.SentenceNumber].Add(node);
            }

            // create spans for each sentence
            Dictionary<int, List<Span>> sentenceSpans = new Dictionary<int, List<Span>>();
            foreach (int sentNum in sentenceNodes.Keys)
            {
                // create span for each set of contiguous nodes
                List<Span> spans = new List<Span>();
                foreach (List<TreeBankNode> nodes in TreeBankNode.GetContiguousNodes(sentenceNodes[sentNum]))
                    spans.Add(new Span(nodes[0].FirstToken.TokenNumber, nodes[nodes.Count - 1].LastToken.TokenNumber));

                sentenceSpans.Add(sentNum, spans);
            }

            return sentenceSpans;
        }

        /// <summary>
        /// Gets nicely formatted string for relation
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _relation + (_semanticClass != null ? "." + _semanticClass : "");
        }
    }
}
