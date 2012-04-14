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
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 354.<para/>
    /// Demodulation: For any terms x, y, and z, where z appears somewhere in literal m<sub>i</sub>
    /// and where UNIFY(x,z) = theta:<para/>
    /// <code>
    ///                 x=y,    m1 OR ... OR m<sub>n</sub>[z]
    ///     ------------------------------------------------------------
    ///     SUB(SUBST(theta,x), SUBST(theta,y), m1 OR ... m<sub>n</sub>)
    /// </code>
    /// where SUBST is the usual substitution of a binding list, and SUB(x,y,m) means to
    /// replace x with y everywhere that x occurs within m.<para/>
    /// <para/>
    /// Some additional restrictions/clarifications highlighted in:<para/>
    /// http://logic.stanford.edu/classes/cs157/2008/lectures/lecture15.pdf<br>
    /// 1. Unit Equations Only.<para/>
    /// 2. Variables substituted in Equation Only.<para/>
    /// </summary>
    public class Demodulation : AbstractModulation 
    {
        public Clause Apply(TermEquality assertion, Clause clExpression) 
        {
            Clause altClExpression = null;

            foreach (Literal l1 in clExpression.GetLiterals()) {
                IAtomicSentence altExpression = Apply(assertion, l1.AtomicSentence);
                if (null != altExpression) {
                    // I have an alternative, create a new clause
                    // with the alternative and return
                    IList<Literal> newLits = new List<Literal>();
                    foreach (Literal l2 in clExpression.GetLiterals()) {
                        if (l1.Equals(l2)) {
                            newLits.Add(l1.NewInstance(altExpression));
                        } else {
                            newLits.Add(l2);
                        }
                    }
                    // Only apply demodulation at most once on
                    // each call.
                    altClExpression = new Clause(newLits);
                    altClExpression.SetProofStep(new ProofStepClauseDemodulation(
                            altClExpression, clExpression, assertion));
                    if (clExpression.Immutable) {
                        altClExpression.Immutable = true;
                    }
                    if (!clExpression.IsStandardizedApartCheckRequired()) {
                        altClExpression.SetStandardizedApartCheckNotRequired();
                    }
                    break;
                }
            }

            return altClExpression;
        }

        public IAtomicSentence Apply(TermEquality assertion,
                IAtomicSentence expression) {
            IAtomicSentence altExpression = null;

            IdentifyCandidateMatchingTerm icm = this.GetMatchingSubstitution(assertion
                    .Term1, expression);

            if (null != icm) {
                ITerm replaceWith = substVisitor.Subst(
                        icm.GetMatchingSubstitution(), assertion.Term2);
                // Want to ignore reflexivity axiom situation, i.e. x = x
                if (!icm.GetMatchingTerm().Equals(replaceWith)) {
                    ReplaceMatchingTerm rmt = new ReplaceMatchingTerm();

                    // Only apply demodulation at most once on each call.
                    altExpression = rmt.Replace(expression, icm.GetMatchingTerm(),
                            replaceWith);
                }
            }

            return altExpression;
        }

        //
        // PROTECTED METHODS
        //
        protected override bool IsValidMatch(ITerm toMatch,
                ISet<Variable> toMatchVariables, ITerm possibleMatch,
                IDictionary<Variable, ITerm> substitution) {
            // Demodulation only allows substitution in the equation only,
            // if the substitution contains variables not in the toMatch
            // side of the equation (i.e. left hand side), then
            // it is not a legal demodulation match.
            // Note: see:
            // http://logic.stanford.edu/classes/cs157/2008/lectures/lecture15.pdf
            // slide 23 for an example.
            if (substitution.Keys.All(toMatchVariables.Contains)) 
            {
                return true;
            }

            return false;
        }
    }

}
