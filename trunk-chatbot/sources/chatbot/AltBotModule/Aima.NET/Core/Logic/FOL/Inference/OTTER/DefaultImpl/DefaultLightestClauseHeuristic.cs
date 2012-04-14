using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference.OTTER.DefaultImpl
{
    using Aima.Core.Logic.FOL.KB.Data;

    public class DefaultLightestClauseHeuristic : ILightestClauseHeuristic 
    {
        private SortedSet<Clause> sos = new SortedSet<Clause>(new LightestClauseSorter());

        //
        // START-LightestClauseHeuristic
        public Clause GetLightestClause() 
        {
            Clause lightest = null;

            if (sos.Count > 0) 
            {
                lightest = sos.First();
            }

            return lightest;
        }

        public void InitialSOS(ISet<Clause> clauses) 
        {
            sos.Clear();
            sos.UnionWith(clauses);
        }

        public void AddedClauseToSOS(Clause clause) 
        {
            sos.Add(clause);
        }

        public void RemovedClauseFromSOS(Clause clause) 
        {
            sos.Remove(clause);
        }

    }

    class LightestClauseSorter : IComparer<Clause> 
    {
        public int Compare(Clause c1, Clause c2) 
        {
            if (c1 == c2) 
            {
                return 0;
            }
            int c1Val = c1.GetNumberLiterals();
            int c2Val = c2.GetNumberLiterals();
            return (c1Val < c2Val ? -1
                    : (c1Val == c2Val ? (this.CompareEqualityIdentities(c1, c2)) : 1));
        }

        private int CompareEqualityIdentities(Clause c1, Clause c2) 
        {
            int c1Len = c1.EqualityIdentity.Length;
            int c2Len = c2.EqualityIdentity.Length;

            return (c1Len < c2Len ? -1 : (c1Len == c2Len ? c1.EqualityIdentity
                    .CompareTo(c2.EqualityIdentity) : 1));
        }
    }
}
