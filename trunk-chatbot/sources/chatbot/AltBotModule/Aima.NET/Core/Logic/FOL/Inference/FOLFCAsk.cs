using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference
{
    using System.Collections;

    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 9.3, page 332.
    /// 
    /// <code><![CDATA[
    /// function FOL-FC-ASK(KB, alpha) returns a substitution or false
    ///   inputs: KB, the knowledge base, a set of first order definite clauses
    ///           alpha, the query, an atomic sentence
    ///   local variables: new, the new sentences inferred on each iteration
    ///   
    ///   repeat until new is empty
    ///      new <- {}
    ///      for each rule in KB do
    ///          (p1 ^ ... ^ pn => q) <- STANDARDIZE-VARAIBLES(rule)
    ///          for each theta such that SUBST(theta, p1 ^ ... ^ pn) = SUBST(theta, p'1 ^ ... ^ p'n)
    ///                         for some p'1,...,p'n in KB
    ///              q' <- SUBST(theta, q)
    ///              if q' does not unify with some sentence already in KB or new then
    ///                   add q' to new
    ///                   theta <- UNIFY(q', alpha)
    ///                   if theta is not fail then return theta
    ///      add new to KB
    ///   return false
    /// ]]></code>
    /// 
    /// Figure 9.3 A conceptually straightforward, but very inefficient forward-chaining algo-
    /// rithm. On each iteration, it adds to KB all the atomic sentences that can be inferred in one
    /// step from the implication sentences and the atomic sentences already in KB. The function
    /// STANDARDIZE-VARIABLES replaces all variables in its arguments with new ones that have
    /// not been used before.
    /// </summary>
    public class FOLFCAsk : IInferenceProcedure 
    {
        /// <summary>
        /// <code>
        /// function FOL-FC-ASK(KB, alpha) returns a substitution or false
        ///   inputs: KB, the knowledge base, a set of first order definite clauses
        ///           alpha, the query, an atomic sentence
        /// </code>
        /// </summary>
        /// <param name="KB"></param>
        /// <param name="query"></param>
        /// <returns></returns>
        public IInferenceResult Ask(FOLKnowledgeBase KB, ISentence query) {
            // Assertions on the type of queries this Inference procedure
            // supports
            if (!(query is IAtomicSentence)) {
                throw new ArgumentOutOfRangeException("query", "Only Atomic Queries are supported.");
            }

            var ansHandler = new FCAskAnswerHandler();

            var alpha = new Literal((IAtomicSentence) query);

            // local variables: new, the new sentences inferred on each iteration
            IList<Literal> newSentences = new List<Literal>();

            // Ensure query is not already a know fact before
            // attempting forward chaining.
            ISet<IDictionary<Variable, ITerm>> answers = KB.Fetch(alpha);
            if (answers.Count > 0) {
                ansHandler.AddProofStep(new ProofStepFoChAlreadyAFact(alpha));
                ansHandler.SetAnswers(answers);
                return ansHandler;
            }

            // repeat until new is empty
            do {

                // new <- {}
                newSentences.Clear();
                // for each rule in KB do
                // (p1 ^ ... ^ pn => q) <-STANDARDIZE-VARIABLES(rule)
                foreach (Clause impl in KB.GetAllDefiniteClauseImplications()) {
                    var standardizedImpl = KB.StandardizeApart(impl);
                    // for each theta such that SUBST(theta, p1 ^ ... ^ pn) =
                    // SUBST(theta, p'1 ^ ... ^ p'n)
                    // --- for some p'1,...,p'n in KB
                    foreach (IDictionary<Variable, ITerm> theta in KB.Fetch(this.Invert(standardizedImpl.GetNegativeLiterals()))) 
                    {
                        // q' <- SUBST(theta, q)
                        Literal qPrime = KB.Subst(theta, standardizedImpl.GetPositiveLiterals()[0]);
                        // if q' does not unify with some sentence already in KB or
                        // new then do
                        if (!KB.IsRenaming(qPrime)
                                && !KB.IsRenaming(qPrime, newSentences)) {
                            // add q' to new
                            newSentences.Add(qPrime);
                            ansHandler.AddProofStep(standardizedImpl, qPrime, theta);
                            // theta <- UNIFY(q', alpha)

                            // if theta is not fail then return theta
                            if (KB.Unify(qPrime.AtomicSentence, alpha.AtomicSentence) != null) {
                                foreach (Literal l in newSentences) {
                                    ISentence s;
                                    if (l.IsPositiveLiteral()) {
                                        s = l.AtomicSentence;
                                    } else {
                                        s = new NotSentence(l.AtomicSentence);
                                    }
                                    KB.tell(s);
                                }
                                ansHandler.SetAnswers(KB.Fetch(alpha));
                                return ansHandler;
                            }
                        }
                    }
                }
                // add new to KB
                foreach (Literal l in newSentences) 
                {
                    ISentence s;
                    if (l.IsPositiveLiteral()) {
                        s = l.AtomicSentence;
                    } else {
                        s = new NotSentence(l.AtomicSentence);
                    }
                    KB.tell(s);
                }
            } while (newSentences.Count > 0);

            // return false
            return ansHandler;
        }

        // END-InferenceProcedure
        //

        //
        // PRIVATE METHODS
        //
        private IList<Literal> Invert(IList<Literal> lits) {
            IList<Literal> invLits = new List<Literal>();
            foreach (Literal l in lits) 
            {
                invLits.Add(new Literal(l.AtomicSentence, (l.IsPositiveLiteral() ? true : false)));
            }
            return invLits;
        }

        class FCAskAnswerHandler : IInferenceResult 
        {

            private IProofStep stepFinal = null;
            private IList<IProof> proofs = new List<IProof>();

            public bool IsPossiblyFalse() {
                return proofs.Count == 0;
            }

            public bool IsTrue() {
                return proofs.Count > 0;
            }

            public bool IsUnknownDueToTimeout() {
                return false;
            }

            public bool IsPartialResultDueToTimeout() {
                return false;
            }

            public IList<IProof> GetProofs() {
                return proofs;
            }

            public void AddProofStep(Clause implication, Literal fact,
                    IDictionary<Variable, ITerm> bindings) {
                stepFinal = new ProofStepFoChAssertFact(implication, fact,
                        bindings, stepFinal);
            }

            public void AddProofStep(IProofStep step) {
                stepFinal = step;
            }

            public void SetAnswers(ISet<IDictionary<Variable, ITerm>> answers) {
                foreach (IDictionary<Variable, ITerm> ans in answers) {
                    proofs.Add(new ProofFinal(stepFinal, ans));
                }
            }
        }
    }

}
