using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.KB.Data
{
    using System.Collections.ObjectModel;

    /// <summary>
    /// Conjunctive Normal Form (CNF) : a conjunction of clauses, where each 
    /// clause is a disjunction of literals.
    /// </summary>
    public class CNF
    {
        private IList<Clause> conjunctionOfClauses;

        public CNF(IList<Clause> conjunctionOfClauses) 
        {
            this.conjunctionOfClauses = conjunctionOfClauses;
        }

        public int GetNumberOfClauses() 
        {
            return conjunctionOfClauses.Count;
        }

        public IList<Clause> GetConjunctionOfClauses() 
        {
            return new ReadOnlyCollection<Clause>(conjunctionOfClauses);
        }

        public override string ToString() 
        {
            var sb = new StringBuilder();
            for (var i = 0; i < this.conjunctionOfClauses.Count; i++)
            {
                if (i > 0)
                {
                    sb.Append(",");
                }

                sb.Append(this.conjunctionOfClauses[i].ToString());
            }

            return sb.ToString();
        }
    }
}
