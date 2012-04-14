using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using System.Data;

    /// <summary>
    /// An assignment assigns values to some or all variables of a CSP.
    /// </summary>
    public class Assignment 
    {
        /// <summary>
        /// Contains all assigned variables. Positions reflect the the order in which
        /// the variables were assigned to values.
        /// </summary>
        IList<Variable> variables;
        /** Maps variables to their assigned values. */
        Dictionary<Variable, object> variableToValue;

        public Assignment() {
            variables = new List<Variable>();
            variableToValue = new Dictionary<Variable, object>();
        }

        public object GetAssignment(Variable var) {
            return variableToValue[var];
        }

        public void SetAssignment(Variable var, object value) {
            if (!variableToValue.ContainsKey(var))
                variables.Add(var);
            variableToValue[var] = value;
        }

        public void RemoveAssignment(Variable var) 
        {
            if (this.HasAssignmentFor(var)) {
                variables.Remove(var);
                variableToValue.Remove(var);
            }
        }

        public bool HasAssignmentFor(Variable var) {
            return variableToValue[var] != null;
        }

        /// <summary>
        /// Returns true if this assignment does not violate any constraints of
        /// <code>constraints</code>.
        /// </summary>
        /// <param name="constraints"></param>
        /// <returns></returns>
        public bool IsConsistent(IList<IConstraint> constraints)
        {
            return constraints.All(cons => cons.IsSatisfiedWith(this));
        }

        /// <summary>
        /// Returns true if this assignment assigns values to every variable of
        /// <code>vars</code>.
        /// </summary>
        /// <param name="vars"></param>
        /// <returns></returns>
        public bool IsComplete(IList<Variable> vars)
        {
            return vars.All(this.HasAssignmentFor);
        }

        /// <summary>
        /// Returns true if this assignment assigns values to every variable of
        /// <code>vars</code>.
        /// </summary>
        /// <param name="vars"></param>
        /// <returns></returns>
        public bool IsComplete(Variable[] vars)
        {
            return vars.All(this.HasAssignmentFor);
        }

        /// <summary>
        /// Returns true if this assignment is consistent as well as complete with
        /// respect to the given CSP.
        /// </summary>
        /// <param name="csp"></param>
        /// <returns></returns>
        public bool IsSolution(CSProblem csp) {
            return this.IsConsistent(csp.GetConstraints())
                    && this.IsComplete(csp.Variables);
        }

        public Assignment Copy() {
            var copy = new Assignment();
            foreach (Variable var in variables) {
                copy.SetAssignment(var, variableToValue[var]);
            }
            return copy;
        }

        public override string ToString()
        {
            var comma = false;
            var result = new StringBuilder("{");
            foreach (var var in this.variables)
            {
                if (comma)
                {
                    result.Append(", ");
                }
                result.Append(var + "=" + this.variableToValue[var]);
                comma = true;
            }
            result.Append("}");
            return result.ToString();
        }
    }
}
