using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    public class NodeExpander
    {
        public static readonly string MetricNodesExpanded = "nodesExpanded";

        protected internal Metrics Metrics { get; private set; }

        public NodeExpander() 
        {
            this.Metrics = new Metrics();
        }

        public virtual void ClearInstrumentation() {
            this.Metrics.Set(MetricNodesExpanded, 0);
        }

        public int GetNodesExpanded() {
            return this.Metrics.GetInt(MetricNodesExpanded);
        }

        public IList<Node> ExpandNode(Node node, Problem problem) 
        {
            IList<Node> childNodes = new List<Node>();

            IActionsFunction actionsFunction = problem.ActionsFunction;
            IResultFunction resultFunction = problem.ResultFunction;
            IStepCostFunction stepCostFunction = problem.StepCostFunction;

            foreach (IAction action in actionsFunction.Actions(node.State)) {
                object successorState = resultFunction.Result(node.State, action);

                double stepCost = stepCostFunction.C(node.State, action, successorState);
                childNodes.Add(new Node(successorState, node, action, stepCost));
            }
            this.Metrics.Set(MetricNodesExpanded, this.Metrics.GetInt(MetricNodesExpanded) + 1);

            return childNodes;
        }
    }
}
