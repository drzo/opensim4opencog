using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Ed.): Figure 6.5, Page 215.
    /// 
    /// <code>
    /// function BACKTRACKING-SEARCH(csp) returns a solution, or failure
    ///    return BACKTRACK({ }, csp)
    /// 
    /// function BACKTRACK(assignment, csp) returns a solution, or failure
    ///    if assignment is complete then return assignment
    ///    var = SELECT-UNASSIGNED-VARIABLE(csp)
    ///    for each value in ORDER-DOMAIN-VALUES(var, assignment, csp) do
    ///       if value is consistent with assignment then
    ///          add {var = value} to assignment
    ///          inferences = INFERENCE(csp, var, value)
    ///          if inferences != failure then
    ///             add inferences to assignment
    ///             result = BACKTRACK(assignment, csp)
    ///             if result != failure then
    ///                return result
    ///          remove {var = value} and inferences from assignment
    ///    return failure
    /// </code>
    /// 
    /// Figure 6.5 A simple backtracking algorithm for constraint satisfaction
    /// problems. The algorithm is modeled on the recursive depth-first search of
    /// Chapter 3. By varying the functions SELECT-UNASSIGNED-VARIABLE and
    /// ORDER-DOMAIN-VALUES, we can implement the general-purpose heuristic discussed
    /// in the text. The function INFERENCE can optionally be used to impose arc-,
    /// path-, or k-consistency, as desired. If a value choice leads to failure
    /// (noticed wither by INFERENCE or by BACKTRACK), then value assignments
    /// (including those made by INFERENCE) are removed from the current assignment
    /// and a new value is tried.
    /// </summary>
    public class BacktrackingStrategy : SolutionStrategy {

        public override Assignment Solve(CSProblem csp) 
        {
            return this.RecursiveBackTrackingSearch(csp, new Assignment());
        }

        /// <summary>
        /// Template method, which can be configured by overriding the three
        /// primitive operations below.
        /// </summary>
        /// <param name="csp"></param>
        /// <param name="assignment"></param>
        /// <returns></returns>
        private Assignment RecursiveBackTrackingSearch(CSProblem csp,
                Assignment assignment) {
            Assignment result = null;
            if (assignment.IsComplete(csp.Variables)) {
                result = assignment;
            } else {
                Variable var = this.SelectUnassignedVariable(assignment, csp);
                foreach (object value in this.OrderDomainValues(var, assignment, csp)) {
                    assignment.SetAssignment(var, value);
                    this.FireStateChanged(assignment, csp);
                    if (assignment.IsConsistent(csp.GetConstraints(var))) {
                        DomainRestoreInfo info = this.Inference(var, assignment, csp);
                        if (!info.IsEmpty())
                            this.FireStateChanged(csp);
                        if (!info.IsEmptyDomainFound()) {
                            result = this.RecursiveBackTrackingSearch(csp, assignment);
                            if (result != null)
                                break;
                        }
                        csp.RestoreDomains(info);
                    }
                    assignment.RemoveAssignment(var);
                }
            }
            return result;
        }

        /// <summary>
        /// Primitive operation, selecting a not yet assigned variable. This default
        /// implementation just selects the first in the ordered list of variables
        /// provided by the CSProblem.
        /// </summary>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        protected virtual Variable SelectUnassignedVariable(Assignment assignment, CSProblem csp)
        {
            return csp.Variables.FirstOrDefault(var => !(assignment.HasAssignmentFor(var)));
        }

        /// <summary>
        /// Primitive operation, ordering the domain values of the specified
        /// variable. This default implementation just takes the default order
        /// provided by the CSProblem.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns></returns>
        protected virtual IEnumerable<object> OrderDomainValues(Variable var,
                Assignment assignment, CSProblem csp) {
            return csp.GetDomain(var);
        }

        /// <summary>
        /// Primitive operation, which tries to prune out values from the CSProblem which
        /// are not possible anymore when extending the given assignment to a
        /// solution. This default implementation just leaves the original CSProblem as it
        /// is.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="assignment"></param>
        /// <param name="csp"></param>
        /// <returns>An object which provides informations about (1) whether changes
        ///         have been performed, (2) possibly inferred empty domains , and
        /// (3) how to restore the domains.</returns>
        protected virtual DomainRestoreInfo Inference(Variable var, Assignment assignment,
                CSProblem csp)
        {
            return new DomainRestoreInfo().Compactify();
        }
    }

}
