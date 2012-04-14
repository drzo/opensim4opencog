using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// Represents a binary constraint which forbids equal values.
    /// </summary>
    public class NotEqualConstraint : IConstraint 
    {

        private Variable var1;
        private Variable var2;
        private IList<Variable> scope;

        public NotEqualConstraint(Variable var1, Variable var2) 
        {
            this.var1 = var1;
            this.var2 = var2;
            scope = new List<Variable>(2);
            scope.Add(var1);
            scope.Add(var2);
        }

        public IList<Variable> GetScope()
        {
            return scope;
        }

        public bool IsSatisfiedWith(Assignment assignment) 
        {
            object value1 = assignment.GetAssignment(var1);
            return value1 == null || !value1.Equals(assignment.GetAssignment(var2));
        }
    }

}
