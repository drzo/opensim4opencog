using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using Aima.Core.Util.Datastructure;

    public class ImprovedBacktrackingStrategy : BacktrackingStrategy 
    {
        protected Selection SelectionStrategy = Selection.DefaultOrder;
        protected InferenceType InferenceStrategy = InferenceType.None;
        protected bool IsLcvHeuristicEnabled;

        /// <summary>
        /// Selects the algorithm for SELECT-UNASSIGNED-VARIABLE
        /// </summary>
        /// <param name="sStrategy"></param>
        public void SetVariableSelection(Selection sStrategy) 
        {
            this.SelectionStrategy = sStrategy;
        }

        /// <summary>
        /// Selects the algorithm for INFERENCE.
        /// </summary>
        /// <param name="iStrategy"></param>
        public void SetInference(InferenceType iStrategy) 
        {
            this.InferenceStrategy = iStrategy;
        }

        /// <summary>
        /// Selects the least constraining value heuristic as implementation for
        /// ORDER-DOMAIN-VALUES.
        /// </summary>
        /// <param name="state"></param>
        public void EnableLcv(bool state) 
        {
            this.IsLcvHeuristicEnabled = state;
        }

        /// <summary>
        /// Starts with a constraint propagation if AC-3 is enabled and then calls
        /// the super class implementation.
        /// </summary>
        /// <param name="csp"></param>
        /// <returns></returns>
        public override Assignment Solve(CSProblem csp) 
        {
            if (this.InferenceStrategy == InferenceType.AC3) 
            {
                DomainRestoreInfo info = new AC3Strategy().ReduceDomains(csp);
                if (!info.IsEmpty())
                {
                    FireStateChanged(csp);
                    if (info.IsEmptyDomainFound())
                        return null;
                }
            }
            return base.Solve(csp);
        }

        /// <summary>
        /// Primitive operation, selecting a not yet assigned variable.
        /// </summary>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        protected override Variable SelectUnassignedVariable(Assignment assignment, CSProblem csp) 
        {
            switch (this.SelectionStrategy) 
            {
            case Selection.MRV:
                return this.ApplyMrvHeuristic(csp, assignment)[0];
            case Selection.MRVDeg:
                IList<Variable> vars = this.ApplyMrvHeuristic(csp, assignment);
                return applyDegreeHeuristic(vars, assignment, csp)[0];
            default:
                foreach (Variable var in csp.Variables) 
                {
                    if (!(assignment.HasAssignmentFor(var)))
                        return var;
                }
                    break;
            }
            return null;
        }

        /// <summary>
        /// Primitive operation, ordering the domain values of the specified
        /// variable.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        protected override IEnumerable<object> OrderDomainValues(Variable var,
                Assignment assignment, CSProblem csp) 
        {
            if (!this.IsLcvHeuristicEnabled)
            {
                return csp.GetDomain(var);
            } else {
                return this.ApplyLeastConstrainingValueHeuristic(var, csp);
            }
        }

        /// <summary>
        /// Primitive operation, which tries to prune out values from the CSProblem which
        /// are not possible anymore when extending the given assignment to a
        /// solution.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns>An object which provides informations about (1) whether changes
        ///         have been performed, (2) possibly inferred empty domains , and
        ///         (3) how to restore the domains.</returns>
        protected override DomainRestoreInfo Inference(Variable var, Assignment assignment,
                CSProblem csp) {
            switch (this.InferenceStrategy) {
            case InferenceType.ForwardChecking:
                return this.DoForwardChecking(var, assignment, csp);
            case InferenceType.AC3:
                return new AC3Strategy().ReduceDomains(var, assignment.GetAssignment(var), csp);
            default:
                return new DomainRestoreInfo().Compactify();
            }
        }

        // //////////////////////////////////////////////////////////////
        // heuristics for selecting the next unassigned variable and domain ordering

        /// <summary>
        /// Implements the minimum-remaining-values heuristic.
        /// </summary>
        /// <param name="csp"></param>
        /// <param name="assignment"></param>
        /// <returns></returns>
        private IList<Variable> ApplyMrvHeuristic(CSProblem csp, Assignment assignment) {
            IList<Variable> result = new List<Variable>();
            int mrv = int.MaxValue;
            foreach (Variable var in csp.Variables) {
                if (!assignment.HasAssignmentFor(var)) {
                    int num = csp.GetDomain(var).Size();
                    if (num <= mrv) {
                        if (num < mrv) {
                            result.Clear();
                            mrv = num;
                        }
                        result.Add(var);
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Implements the degree heuristic.
        /// </summary>
        /// <param name="vars"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        private IList<Variable> applyDegreeHeuristic(IList<Variable> vars,
                Assignment assignment, CSProblem csp) {
            IList<Variable> result = new List<Variable>();
            int maxDegree = int.MinValue;
            foreach (Variable var in vars) {
                int degree = 0;
                foreach (IConstraint constraint in csp.GetConstraints(var)) {
                    Variable neighbor = csp.GetNeighbor(var, constraint);
                    if (!assignment.HasAssignmentFor(neighbor)
                            && csp.GetDomain(neighbor).Size() > 1)
                        ++degree;
                }
                if (degree >= maxDegree) {
                    if (degree > maxDegree) {
                        result.Clear();
                        maxDegree = degree;
                    }
                    result.Add(var);
                }
            }
            return result;
        }

        /// <summary>
        /// Implements the least constraining value heuristic.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        private IList<object> ApplyLeastConstrainingValueHeuristic(Variable var,
                CSProblem csp) {
            var pairs = (from value in csp.GetDomain(var)
                         let num = this.CountLostValues(var, value, csp)
                         select new Pair<object, int>(value, num)).ToList();
            pairs.Sort((o1, o2) => o1.GetSecond() < o2.GetSecond() ? -1 : o1.GetSecond() > o2.GetSecond() ? 1 : 0);
            IList<object> result = new List<object>();
            foreach (Pair<object, int> pair in pairs)
                result.Add(pair.GetFirst());
            return result;
        }

        private int CountLostValues(Variable var, object value, CSProblem csp)
        {
            int result = 0;
            Assignment assignment = new Assignment();
            assignment.SetAssignment(var, value);
            foreach (IConstraint constraint in csp.GetConstraints(var)) {
                Variable neighbor = csp.GetNeighbor(var, constraint);
                foreach (object nValue in csp.GetDomain(neighbor)) {
                    assignment.SetAssignment(neighbor, nValue);
                    if (!constraint.IsSatisfiedWith(assignment)) {
                        ++result;
                    }
                }
            }
            return result;
        }

        // //////////////////////////////////////////////////////////////
        // inference algorithms

        /// <summary>
        /// Implements forward checking.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        private DomainRestoreInfo DoForwardChecking(Variable var,
                Assignment assignment, CSProblem csp) 
        {
            DomainRestoreInfo result = new DomainRestoreInfo();
            foreach (IConstraint constraint in csp.GetConstraints(var)) {
                IList<Variable> scope = constraint.GetScope();
                if (scope.Count == 2) {
                    foreach (Variable neighbor in constraint.GetScope()) {
                        if (!assignment.HasAssignmentFor(neighbor)) {
                            if (this.Revise(neighbor, constraint, assignment, csp,
                                    result)) {
                                if (csp.GetDomain(neighbor).IsEmpty()) {
                                    result.SetEmptyDomainFound(true);
                                    return result;
                                }
                            }
                        }
                    }
                }
            }
            return result;
        }

        private bool Revise(Variable var, IConstraint constraint,
                Assignment assignment, CSProblem csp, DomainRestoreInfo info) {

            bool revised = false;
            foreach (object value in csp.GetDomain(var)) {
                assignment.SetAssignment(var, value);
                if (!constraint.IsSatisfiedWith(assignment)) {
                    info.StoreDomainFor(var, csp.GetDomain(var));
                    csp.RemoveValueFromDomain(var, value);
                    revised = true;
                }
                assignment.RemoveAssignment(var);
            }
            return revised;
        }

        // //////////////////////////////////////////////////////////////
        // two enumerations

        public enum Selection {
            DefaultOrder, MRV, MRVDeg
        }

        public enum InferenceType 
        {
            None, ForwardChecking, AC3
        }
    }

}
