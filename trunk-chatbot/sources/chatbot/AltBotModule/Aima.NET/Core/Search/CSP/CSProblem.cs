using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using System.Collections.ObjectModel;

    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Ed.): Section 6.1, Page 202. A
    /// constraint satisfaction problem or CSP consists of three components, X, D,
    /// and C:
    /// <ul>
    /// <li>X is a set of variables, {X1, ... ,Xn}.</li>
    /// <li>D is a set of domains, {D1, ... ,Dn}, one for each variable.</li>
    /// <li>C is a set of constraints that specify allowable combinations of values.</li>
    /// </ul>
    /// </summary>
    public class CSProblem
    {

        private IList<Variable> variables;
        private IList<Domain> domains;
        private IList<IConstraint> constraints;

        public IList<Variable> Variables
        {
            get
            {
                return new ReadOnlyCollection<Variable>(variables);
            }
        }

        /// <summary>
        /// Lookup, which maps a variable to its index in the list of variables.
        /// </summary>
        private Dictionary<Variable, int> varIndexHash;
        /// <summary>
        /// IConstraint network. Maps variables to those constraints in which they
        /// participate.
        /// </summary>
        private Dictionary<Variable, IList<IConstraint>> cnet;

        private CSProblem()
        {
        }

        /// <summary>
        /// Creates a new CSP for a fixed set of variables.
        /// </summary>
        /// <param name="vars"></param>
        public CSProblem(IList<Variable> vars) {
        variables = new List<Variable>(vars.Count);
        domains = new List<Domain>(vars.Count);
        constraints = new List<IConstraint>();
        varIndexHash = new Dictionary<Variable, int>();
        cnet = new Dictionary<Variable, IList<IConstraint>>();
        var emptyDomain = new Domain(new List<object>(0));
        int index = 0;
        foreach (Variable var in vars) {
            variables.Add(var);
            domains.Add(emptyDomain);
            varIndexHash[var] = index++;
            cnet[var] = new List<IConstraint>();
        }
    }

        public int IndexOf(Variable var)
        {
            return varIndexHash[var];
        }

        public Domain GetDomain(Variable var)
        {
            return domains[varIndexHash[var]];
        }

        public void SetDomain(Variable var, Domain domain)
        {
            domains[this.IndexOf(var)] = domain;
        }

        public void RemoveValueFromDomain(Variable var, object value) 
        {
            Domain currDomain = this.GetDomain(var);
            IList<object> values = new List<object>(currDomain.Count());
            foreach (object v in currDomain)
                if (!v.Equals(value))
                    values.Add(v);
            this.SetDomain(var, new Domain(values));
        }

        public void RestoreDomains(DomainRestoreInfo info) {
            foreach (Pair<Variable, Domain> pair in info.SavedDomains)
                this.SetDomain(pair.GetFirst(), pair.GetSecond());
        }

        public IList<IConstraint> GetConstraints()
        {
            return constraints;
        }

        /// <summary>
        /// Returns all constraints in which the specified variable participates.
        /// </summary>
        /// <param name="var"></param>
        /// <returns></returns>
        public IList<IConstraint> GetConstraints(Variable var)
        {
            return cnet[var];
        }

        public void AddConstraint(IConstraint constraint) 
        {
            constraints.Add(constraint);
            foreach (Variable var in constraint.GetScope())
                cnet[var].Add(constraint);
        }

        /// <summary>
        /// Returns for binary constraints the other variable from the scope.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="constraint"></param>
        /// <returns>a variable or null for non-binary constraints.</returns>
        public Variable GetNeighbor(Variable var, IConstraint constraint)
        {
            IList<Variable> scope = constraint.GetScope();
            if (scope.Count == 2)
            {
                if (var == scope[0])
                    return scope[1];
                if (var == scope[1])
                    return scope[0];
            }
            return null;
        }

        /// <summary>
        /// Returns a copy which contains a copy of the domains list and is in all
        /// other aspects a flat copy of this.
        /// </summary>
        /// <returns></returns>
        public CSProblem CopyDomains()
        {
            CSProblem result = new CSProblem();
            result.variables = variables;
            result.domains = new List<Domain>(domains.Count);
            result.domains = domains.ToList();
            result.constraints = constraints;
            result.varIndexHash = varIndexHash;
            result.cnet = cnet;
            return result;
        }
    }
}
