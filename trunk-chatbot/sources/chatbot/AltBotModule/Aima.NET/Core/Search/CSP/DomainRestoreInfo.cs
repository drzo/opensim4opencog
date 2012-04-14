using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.CSP
{
    using Aima.Core.Util.Datastructure;

    public class DomainRestoreInfo
    {
        public IList<Pair<Variable, Domain>> SavedDomains { get; private set; }
        private HashSet<Variable> affectedVariables;
        private bool emptyDomainObserved;

        public DomainRestoreInfo()
        {
            this.SavedDomains = new List<Pair<Variable, Domain>>();
            affectedVariables = new HashSet<Variable>();
        }

        public void Clear()
        {
            this.SavedDomains.Clear();
            affectedVariables.Clear();
        }

        public bool IsEmpty()
        {
            return this.SavedDomains.Count == 0;
        }

        /// <summary>
        /// Stores the specified domain for the specified variable if a domain has
        /// not yet been stored for the variable.
        /// </summary>
        /// <param name="var"></param>
        /// <param name="domain"></param>
        public void StoreDomainFor(Variable var, Domain domain)
        {
            if (!affectedVariables.Contains(var))
                this.SavedDomains.Add(new Pair<Variable, Domain>(var, domain));
        }

        public void SetEmptyDomainFound(bool b)
        {
            emptyDomainObserved = b;
        }


        /// <summary>
        /// Can be called after all domain information has been collected to reduce
        /// storage consumption.
        /// </summary>
        /// <returns>this object, after removing one hashtable.</returns>
        public DomainRestoreInfo Compactify()
        {
            affectedVariables = null;
            return this;
        }

        public bool IsEmptyDomainFound()
        {
            return emptyDomainObserved;
        }
    }
}
