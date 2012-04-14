using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Informed
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.26, page 99.
    /// 
    /// <code><![CDATA[
    /// function RECURSIVE-BEST-FIRST-SEARCH(problem) returns a solution, or failure
    ///   return RBFS(problem, MAKE-NODE(problem.INITIAL-STATE), infinity)
    ///   
    /// function RBFS(problem, node, f_limit) returns a solution, or failure and a new f-cost limit
    ///   if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
    ///   successors <- []
    ///   for each action in problem.ACTION(node.STATE) do
    ///       add CHILD-NODE(problem, node, action) into successors
    ///   if successors is empty then return failure, infinity
    ///   for each s in successors do // update f with value from previous search, if any
    ///     s.f <- max(s.g + s.h, node.f)
    ///   repeat
    ///     best <- the lowest f-value node in successors
    ///     if best.f > f_limit then return failure, best.f
    ///     alternative <- the second-lowest f-value among successors
    ///     result, best.f <- RBFS(problem, best, min(f_limit, alternative))
    ///     if result != failure then return result
    /// ]]></code>
    /// Figure 3.26 The algorithm for recursive best-first search.
    /// </summary>
    public class RecursiveBestFirstSearch : NodeExpander, ISearch 
    {

        private readonly IEvaluationFunction evaluationFunction;

        private static readonly string MaxRecursiveDepth = "maxRecursiveDepth";

        private static readonly string PathCost = "pathCost";

        private static readonly double Infinity = double.MaxValue;

        public RecursiveBestFirstSearch(IEvaluationFunction ef) {
            evaluationFunction = ef;
        }

        // function RECURSIVE-BEST-FIRST-SEARCH(problem) returns a solution, or
        // failure
        public IList<IAction> Search(Problem p) 
        {
            IList<IAction> actions = new List<IAction>();

            ClearInstrumentation();

            // RBFS(problem, MAKE-NODE(INITIAL-STATE[problem]), infinity)
            Node n = new Node(p.InitialState);
            SearchResult sr = this.Rbfs(p, n, evaluationFunction.F(n), Infinity, 0);
            if (sr.GetOutcome() == SearchResult.SearchOutcome.SolutionFound) {
                Node s = sr.GetSolution();
                actions = SearchUtils.ActionsFromNodes(s.GetPathFromRoot());
                this.SetPathCost(s.PathCost);
            }

            // Empty IList can indicate already at Goal
            // or unable to find valid Set of actions
            return actions;
        }

        public Metrics GetMetrics()
        {
            return this.Metrics;
        }

        public override void ClearInstrumentation() 
        {
            base.ClearInstrumentation();
            Metrics.Set(MaxRecursiveDepth, 0);
            Metrics.Set(PathCost, 0.0);
        }

        public void SetMaxRecursiveDepth(int recursiveDepth) {
            int maxRdepth = Metrics.GetInt(MaxRecursiveDepth);
            if (recursiveDepth > maxRdepth) {
                Metrics.Set(MaxRecursiveDepth, recursiveDepth);
            }
        }

        public int GetMaxRecursiveDepth() {
            return Metrics.GetInt(MaxRecursiveDepth);
        }

        public double GetPathCost() {
            return Metrics.GetDouble(PathCost);
        }

        public void SetPathCost(double pathCost) {
            Metrics.Set(PathCost, pathCost);
        }

        // function RBFS(problem, node, f_limit) returns a solution, or failure and
        // a new f-cost limit
        private SearchResult Rbfs(Problem p, Node n, double node_f, double fLimit,int recursiveDepth) 
        {

            this.SetMaxRecursiveDepth(recursiveDepth);

            // if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
            if (SearchUtils.IsGoalState(p, n)) {
                return new SearchResult(n, fLimit);
            }

            // successors <- []
            // for each action in problem.ACTION(node.STATE) do
            // add CHILD-NODE(problem, node, action) into successors
            IList<Node> successors = ExpandNode(n, p);
            // if successors is empty then return failure, infinity
            if (0 == successors.Count)
            {
                return new SearchResult(null, Infinity);
            }
            double[] f = new double[successors.Count];
            // for each s in successors do
            // update f with value from previous search, if any
            int size = successors.Count;
            for (int s = 0; s < size; s++) {
                // s.f <- max(s.g + s.h, node.f)
                f[s] = Math.Max(evaluationFunction.F(successors[s]), node_f);
            }

            // repeat
            while (true) {
                // best <- the lowest f-value node in successors
                int bestIndex = this.GetBestFValueIndex(f);
                // if best.f > f_limit then return failure, best.f
                if (f[bestIndex] > fLimit) {
                    return new SearchResult(null, f[bestIndex]);
                }
                // if best.f > f_limit then return failure, best.f
                int altIndex = this.GetNextBestFValueIndex(f, bestIndex);
                // result, best.f <- RBFS(problem, best, min(f_limit, alternative))
                SearchResult sr = this.Rbfs(p, successors[bestIndex], f[bestIndex],
                        Math.Min(fLimit, f[altIndex]), recursiveDepth + 1);
                f[bestIndex] = sr.GetFCostLimit();
                // if result != failure then return result
                if (sr.GetOutcome() == SearchResult.SearchOutcome.SolutionFound) {
                    return sr;
                }
            }
        }

        // the lowest f-value node
        private int GetBestFValueIndex(double[] f) {
            int lidx = 0;
            double lowestSoFar = Infinity;

            for (int i = 0; i < f.Length; i++) {
                if (f[i] < lowestSoFar) {
                    lowestSoFar = f[i];
                    lidx = i;
                }
            }

            return lidx;
        }

        // the second-lowest f-value
        private int GetNextBestFValueIndex(double[] f, int bestIndex)
        {
            // Array may only contain 1 item (i.e. no alternative),
            // therefore default to bestIndex initially
            int lidx = bestIndex;
            double lowestSoFar = Infinity;

            for (int i = 0; i < f.Length; i++)
            {
                if (i != bestIndex && f[i] < lowestSoFar)
                {
                    lowestSoFar = f[i];
                    lidx = i;
                }
            }

            return lidx;
        }
    }

    class SearchResult {
        public enum SearchOutcome {
            Failure, SolutionFound
        };

        private Node solution;

        private SearchOutcome outcome;

        private readonly double fCostLimit;

        public SearchResult(Node solution, double fCostLimit) 
        {
            if (null == solution) {
                this.outcome = SearchOutcome.Failure;
            } else {
                this.outcome = SearchOutcome.SolutionFound;
                this.solution = solution;
            }
            this.fCostLimit = fCostLimit;
        }

        public SearchOutcome GetOutcome() {
            return outcome;
        }

        public Node GetSolution() {
            return solution;
        }

        public double GetFCostLimit() {
            return fCostLimit;
        }
    }
}
