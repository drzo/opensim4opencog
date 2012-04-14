using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    ///Artificial Intelligence A Modern Approach (3r Edition): page 354.<para />
    ///
    ///Paramodulation: For any terms x, y, and z, where z appears somewhere in literal m<sub>i</sub>,
    ///and where UNIFY(x,z) = theta,<para />
    ///<code>
    ///                         l1 OR ... l<sub>k</sub> OR x=y,     m1 OR ... OR m<sub>n</sub>
    ///    -------------------------------------------------------------------------------------------------------
    ///    SUB(SUBST(theta,x), SUBST(theta,y), SUBST(theta, l1 OR ... l<sub>k</sub> OR m1 OR ... OR m<sub>n</sub>)
    ///</code>
    ///Paramodulation yields a complete inference procedure for first-order logic with equality.
    /// </summary>
    public class Paramodulation : AbstractModulation 
    {
        private static IStandardizeApartIndexical _saIndexical = 
            StandardizeApartIndexicalFactory.NewStandardizeApartIndexical('p');
        private static IList<Literal> _emptyLiteralList = new List<Literal>();

        private StandardizeApart sApart = new StandardizeApart();

        public ISet<Clause> Apply(Clause c1, Clause c2) 
        {
            return this.Apply(c1, c2, false);
        }

        public ISet<Clause> Apply(Clause c1, Clause c2, bool standardizeApart) 
        {
            ISet<Clause> paraExpressions = new HashedSet<Clause>();

            for (int i = 0; i < 2; i++) {
                Clause topClause, equalityClause;
                if (i == 0) 
                {
                    topClause = c1;
                    equalityClause = c2;
                } 
                else 
                {
                    topClause = c2;
                    equalityClause = c1;
                }

                foreach (Literal possEqLit in equalityClause.GetLiterals()) 
                {
                    // Must be a positive term equality to be used
                    // for paramodulation.
                    if (possEqLit.IsPositiveLiteral()
                            && possEqLit.AtomicSentence is TermEquality) 
                    {
                        var assertion = (TermEquality) possEqLit.AtomicSentence;

                        // Test matching for both sides of the equality
                        for (int x = 0; x < 2; x++) 
                        {
                            ITerm toMatch, toReplaceWith;
                            if (x == 0) {
                                toMatch = assertion.Term1;
                                toReplaceWith = assertion.Term2;
                            } else {
                                toMatch = assertion.Term2;
                                toReplaceWith = assertion.Term1;
                            }

                            foreach (Literal l1 in topClause.GetLiterals()) 
                            {
                                var icm = GetMatchingSubstitution(
                                        toMatch, l1.AtomicSentence);

                                if (icm != null) 
                                {
                                    ITerm replaceWith = substVisitor.Subst(
                                        icm.GetMatchingSubstitution(), toReplaceWith);

                                    // Want to ignore reflexivity axiom situation,
                                    // i.e. x = x
                                    if (icm.GetMatchingTerm().Equals(replaceWith)) 
                                    {
                                        continue;
                                    }

                                    var rmt = new ReplaceMatchingTerm();

                                    IAtomicSentence altExpression = rmt.Replace(l1.AtomicSentence, 
                                        icm.GetMatchingTerm(), replaceWith);

                                    // I have an alternative, create a new clause
                                    // with the alternative and the substitution
                                    // applied to all the literals before returning
                                    var newLits = new List<Literal>();
                                    foreach (Literal l2 in topClause.GetLiterals()) 
                                    {
                                        if (l1.Equals(l2)) 
                                        {
                                            newLits.Add(l1.NewInstance((IAtomicSentence) substVisitor
                                                                    .Subst(icm.GetMatchingSubstitution(),altExpression)));
                                        } 
                                        else 
                                        {
                                            newLits.Add(substVisitor.Subst(icm.GetMatchingSubstitution(),l2));
                                        }
                                    }
                                    // Assign the equality clause literals,
                                    // excluding
                                    // the term equality used.
                                    newLits.AddRange(from l2 in equalityClause.GetLiterals()
                                                     where !possEqLit.Equals(l2)
                                                     select this.substVisitor.Subst(icm.GetMatchingSubstitution(), l2));

                                    // Only apply paramodulation at most once
                                    // for each term equality.
                                    Clause nc;
                                    if (standardizeApart) 
                                    {
                                        sApart.GetStandardizeApartResult(newLits, _emptyLiteralList, _saIndexical);
                                        nc = new Clause(newLits);

                                    } 
                                    else 
                                    {
                                        nc = new Clause(newLits);
                                    }
                                    nc.SetProofStep(new ProofStepClauseParamodulation(nc, topClause, equalityClause, assertion));
                                    if (c1.Immutable)
                                    {
                                        nc.Immutable = true;
                                    }
                                    if (!c1.IsStandardizedApartCheckRequired()) 
                                    {
                                        c1.SetStandardizedApartCheckNotRequired();
                                    }
                                    paraExpressions.Add(nc);
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            return paraExpressions;
        }

        protected override bool IsValidMatch(ITerm toMatch,
                ISet<Variable> toMatchVariables, ITerm possibleMatch,
                IDictionary<Variable, ITerm> substitution) 
        {

            if (possibleMatch != null && substitution != null) {
                // Note:
                // [Brand 1975] showed that paramodulation into
                // variables is unnecessary.
                if (!(possibleMatch is Variable)) 
                {
                    // TODO: Find out whether the following statement from:
                    // http://www.cs.miami.edu/~geoff/Courses/CSC648-07F/Content/
                    // Paramodulation.shtml
                    // is actually the case, as it was not positive but
                    // intuitively makes sense:
                    // "Similarly, depending on how paramodulation is used, it is
                    // often unnecessary to paramodulate from variables."
                    // if (!(toMatch instanceof Variable)) {
                    return true;
                    // }
                }
            }
            return false;
        }
    }

}
