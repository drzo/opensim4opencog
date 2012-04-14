using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    /// <summary>
    /// A constraint specifies the allowable combinations of values for a set of
    /// variables. Each constraint consists of a pair <![CDATA[<scope, rel>]]>, where scope is a
    /// tuple of variables that participate in the constraint and rel is a relation
    /// that defines the values that those variables can take on.
    /// 
    /// Note: Implementations of this interface define the different kinds of
    /// relations that constraints can represent.
    /// </summary>
    public interface IConstraint
    {
        /// <summary>
        /// Returns a tuple of variables that participate in the constraint.
        /// </summary>
        /// <returns></returns>
        IList<Variable> GetScope();

        /// <summary>
        /// Constrains the values that the variables can take on.
        /// </summary>
        /// <param name="assignment"></param>
        /// <returns></returns>
        bool IsSatisfiedWith(Assignment assignment);
    }
}
