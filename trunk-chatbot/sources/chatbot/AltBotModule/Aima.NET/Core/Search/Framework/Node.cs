using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.10, page 79.<br />
    /// 
    /// Figure 3.10 Nodes are the data structures from which the search tree is constructed. Each
    /// has a parent, a state, and various bookkeeping fields. Arrows point from child to parent.<br />
    /// <br />
    /// Search algorithms require a data structure to keep track of the search tree that is being
    /// constructed. For each node n of the tree, we have a structure that contains four components:
    /// <ul>
    /// <li>n.STATE: the state in the state space to which the node corresponds;</li>
    /// <li>n.PARENT: the node in the search tree that generated this node;</li>
    /// <li>n.ACTION: the action that was applied to the parent to generate the node;</li>
    /// <li>n.PATH-COST: the cost, traditionally denoted by g(n), of the path from the initial state
    /// to the node, as indicated by the parent pointers.</li>
    /// </ul>
    /// </summary>
    public class Node 
    {

        // n.STATE: the state in the state space to which the node corresponds;
        public object State { get; private set; }

        // n.PARENT: the node in the search tree that generated this node;
        public Node Parent { get; private set; }

        // n.ACTION: the action that was applied to the parent to generate the node;
        public IAction Action { get; private set; }

        // n.PATH-COST: the cost, traditionally denoted by g(n), of the path from
        // the initial state to the node, as indicated by the parent pointers.
        public double PathCost { get; private set; }

        public Node(object state) 
        {
            this.State = state;
            this.PathCost = 0.0;
        }

        public Node(object state, Node parent, IAction action, double stepCost): this(state) 
        {
            this.Parent = parent;
            this.Action = action;
            this.PathCost = parent.PathCost + stepCost;
        }

        public bool IsRootNode() {
            return this.Parent == null;
        }

        public IList<Node> GetPathFromRoot()
        {
            IList<Node> path = new List<Node>();
            Node current = this;
            while (!current.IsRootNode())
            {
                path.Insert(0, current);
                current = current.Parent;
            }

            // ensure the root node is added
            path.Insert(0, current);
            return path;
        }

        public override string ToString() {
            return "[parent=" + this.Parent + ", action=" + this.Action + ", state="
                    + this.State + ", pathCost=" + this.PathCost + "]";
        }
    }
}
