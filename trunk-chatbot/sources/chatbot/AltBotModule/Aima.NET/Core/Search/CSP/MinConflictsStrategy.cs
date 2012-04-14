using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using System.Data;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Ed.): Figure 6.8, Page 221.
    /// 
    /// <code>
    /// function MIN-CONFLICTS(csp, max-steps) returns a solution or failure
    ///    inputs: csp, a constraint satisfaction problem
    ///            max-steps, the number of steps allowed before giving up
    ///    current = an initial complete assignment for csp
    ///    for i = 1 to max steps do
    ///       if current is a solution for csp then return current
    ///       var = a randomly chosen conflicted variable from csp.VARIABLES
    ///       value = the value v for var that minimizes CONFLICTS(var, v, current, csp)
    ///       set var = value in current
    ///    return failure
    /// </code>
    /// 
    /// Figure 6.8 The MIN-CONFLICTS algorithm for solving CSPs by local search. The
    /// initial state may be chosen randomly or by a greedy assignment process that
    /// chooses a minimal-conflict value for each variable in turn. The CONFLICTS
    /// function counts the number of constraints violated by a particular value,
    /// given the rest of the current assignment.
    /// </summary>
    public class MinConflictsStrategy : SolutionStrategy 
    {
        private int maxSteps;

        public MinConflictsStrategy(int maxSteps) {
            this.maxSteps = maxSteps;
        }

        public override Assignment Solve(CSProblem csp) {
            Assignment assignment = this.GenerateRandomAssignment(csp);
            FireStateChanged(assignment, csp);
            for (int i = 0; i < maxSteps; i++) 
            {
                if (assignment.IsSolution(csp)) {
                    return assignment;
                }
                IList<Variable> vars = this.GetConflictedVariables(assignment, csp);
                Variable var = Util.Util.SelectRandomlyFromList(vars);
                object value = this.GetMinConflictValueFor(var, assignment, csp);
                assignment.SetAssignment(var, value);
                FireStateChanged(assignment, csp);
            }
            return null;
        }

        private Assignment GenerateRandomAssignment(CSProblem csp) {
            Assignment assignment = new Assignment();
            foreach (Variable var in csp.Variables) {
                object randomValue = Util.Util.SelectRandomlyFromList(csp.GetDomain(var).ToList());
                assignment.SetAssignment(var, randomValue);
            }
            return assignment;
        }

        private IList<Variable> GetConflictedVariables(Assignment assignment, CSProblem csp) 
        {
            IList<Variable> result = new List<Variable>();
            foreach (IConstraint constraint in csp.GetConstraints()) {
                if (!constraint.IsSatisfiedWith(assignment))
                    foreach (Variable var in constraint.GetScope())
                        if (!result.Contains(var))
                            result.Add(var);
            }
            return result;
        }

        private object GetMinConflictValueFor(Variable var, Assignment assignment, CSProblem csp)
        {
            IList<IConstraint> constraints = csp.GetConstraints(var);
            Assignment duplicate = assignment.Copy();
            int minConflict = int.MaxValue;
            IList<object> resultCandidates = new List<object>();
            foreach (object value in csp.GetDomain(var))
            {
                duplicate.SetAssignment(var, value);
                int currConflict = this.CountConflicts(duplicate, constraints);
                if (currConflict <= minConflict)
                {
                    if (currConflict < minConflict)
                    {
                        resultCandidates.Clear();
                        minConflict = currConflict;
                    }
                    resultCandidates.Add(value);
                }
            }
            return resultCandidates.Count != 0 ? Util.Util.SelectRandomlyFromList(resultCandidates) : null;
        }

        private int CountConflicts(Assignment assignment, IList<IConstraint> constraints)
        {
            return constraints.Count(constraint => !constraint.IsSatisfiedWith(assignment));
        }
    }

}
