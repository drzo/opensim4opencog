using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    public class SearchUtils
    {

        public static IList<IAction> ActionsFromNodes(IList<Node> nodeList)
        {
            IList<IAction> actions = new List<IAction>();
            if (nodeList.Count == 1)
            {
                // I'm at the root node, this indicates I started at the
                // Goal node, therefore just return a NoOp
                actions.Add(NoOpAction.NoOp);
            }
            else
            {
                // ignore the root node this has no action
                // hence index starts from 1 not zero
                for (int i = 1; i < nodeList.Count; i++)
                {
                    Node node = nodeList[i];
                    actions.Add(node.Action);
                }
            }
            return actions;
        }

        public static bool IsGoalState(Problem p, Node n) 
        {
            bool isGoal = false;
            IGoalTest gt = p.GoalTest;
            if (gt.IsGoalState(n.State)) 
            {
                if (gt is ISolutionChecker)
                {
                    isGoal = ((ISolutionChecker)gt).IsAcceptableSolution(ActionsFromNodes(n.GetPathFromRoot()), n.State);
                }
                else
                {
                    isGoal = true;
                }
            }
            return isGoal;
        }
    }
}
