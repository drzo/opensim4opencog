using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Uninformed
{
    using System.Collections.ObjectModel;

    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.17, page 88.
    /// 
    /// <code><![CDATA[
    /// function DEPTH-LIMITED-SEARCH(problem, limit) returns a solution, or failure/cutoff
    ///   return RECURSIVE-DLS(MAKE-NODE(problem.INITIAL-STATE), problem, limit)
    ///   
    /// function RECURSIVE-DLS(node, problem, limit) returns a solution, or failure/cutoff
    ///   if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
    ///   else if limit = 0 then return cutoff
    ///   else
    ///       cutoff_occurred? <- false
    ///       for each action in problem.ACTIONS(node.STATE) do
    ///           child <- CHILD-NODE(problem, node, action)
    ///           result <- RECURSIVE-DLS(child, problem, limit - 1)
    ///           if result = cutoff then cutoff_occurred? <- true
    ///           else if result != failure then return result
    ///       if cutoff_occurred? then return cutoff else return failure
    /// ]]></code>
    /// Figure 3.17 A recursive implementation of depth-limited search.
    /// </summary>
    public class DepthLimitedSearch : NodeExpander, ISearch 
    {
        private const string PathCost = "pathCost";
        private static IList<IAction> cutoffResult;
        private readonly int limit;

        public DepthLimitedSearch(int limit) 
        {
            this.limit = limit;
        }

        public bool IsCutOff(IList<IAction> result) {
            return 1 == result.Count
                    && CutOffIndicatorAction.CutOff.Equals(result[0]);
        }

        public bool IsFailure(IList<IAction> result) {
            return 0 == result.Count;
        }

        /// <summary>
        /// function DEPTH-LIMITED-SEARCH(problem, limit) returns a solution, or failure/cutoff
        /// </summary>
        /// <param name="p"></param>
        /// <returns>if goal found, the list of actions to the Goal. If already at the
        /// goal you will receive a IList with a single NoOp IAction in it. If
        /// fail to find the Goal, an empty list will be returned to indicate
        /// that the search failed. If the search was cutoff (i.e. reached
        /// its limit without finding a goal) a IList with one
        /// CutOffIndicatorAction.CUT_OFF in it will be returned (Note: this
        /// is a NoOp action).</returns>
        public IList<IAction> Search(Problem p) 
        {
            ClearInstrumentation();
            // return RECURSIVE-DLS(MAKE-NODE(INITIAL-STATE[problem]), problem,
            // limit)
            return this.RecursiveDls(new Node(p.InitialState), p, limit);
        }

        public Metrics GetMetrics()
        {
            return this.Metrics;
        }

        public override void ClearInstrumentation() {
            base.ClearInstrumentation();
            Metrics.Set(PathCost, 0);
        }

        public double GetPathCost() {
            return Metrics.GetDouble(PathCost);
        }

        public void SetPathCost(Double pathCost) {
            Metrics.Set(PathCost, pathCost);
        }

        // function RECURSIVE-DLS(node, problem, limit) returns a solution, or
        // failure/cutoff
        private IList<IAction> RecursiveDls(Node node, Problem problem, int limit) {
            // if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
            if (SearchUtils.IsGoalState(problem, node)) {
                this.SetPathCost(node.PathCost);
                return SearchUtils.ActionsFromNodes(node.GetPathFromRoot());
            } else if (0 == limit) {
                // else if limit = 0 then return cutoff
                return this.Cutoff();
            } else {
                // else
                // cutoff_occurred? <- false
                bool cutoffOccurred = false;
                // for each action in problem.ACTIONS(node.STATE) do
                foreach (Node child in this.ExpandNode(node, problem)) {
                    // child <- CHILD-NODE(problem, node, action)
                    // result <- RECURSIVE-DLS(child, problem, limit - 1)
                    IList<IAction> result = this.RecursiveDls(child, problem, limit - 1);
                    // if result = cutoff then cutoff_occurred? <- true
                    if (this.IsCutOff(result)) {
                        cutoffOccurred = true;
                    } else if (!this.IsFailure(result)) {
                        // else if result != failure then return result
                        return result;
                    }
                }

                // if cutoff_occurred? then return cutoff else return failure
                if (cutoffOccurred) {
                    return this.Cutoff();
                } else {
                    return this.Failure();
                }
            }
        }

        private IList<IAction> Cutoff()
        {
            // Only want to created once
            if (cutoffResult == null)
            {
                cutoffResult = new List<IAction>();
                cutoffResult.Add(CutOffIndicatorAction.CutOff);
                // Ensure it cannot be modified externally.
                cutoffResult = new ReadOnlyCollection<IAction>(cutoffResult);
            }
            return cutoffResult;
        }

        private IList<IAction> Failure() {
            return new List<IAction>();
        }
    }
}
