using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference
{
    using System.Collections;

    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (2nd Edition): Figure 9.6, page 288.
    /// 
    /// <code><![CDATA[
    /// function FOL-BC-ASK(KB, goals, theta) returns a set of substitutions
    ///   input: KB, a knowledge base
    ///          goals, a list of conjuncts forming a query (theta already applied)
    ///          theta, the current substitution, initially the empty substitution {}
    ///   local variables: answers, a set of substitutions, initially empty
    ///   
    ///   if goals is empty then return {theta}
    ///   qDelta <- SUBST(theta, FIRST(goals))
    ///   for each sentence r in KB where STANDARDIZE-APART(r) = (p1 ^ ... ^ pn => q)
    ///          and thetaDelta <- UNIFY(q, qDelta) succeeds
    ///       new_goals <- [p1,...,pn|REST(goals)]
    ///       answers <- FOL-BC-ASK(KB, new_goals, COMPOSE(thetaDelta, theta)) U answers
    ///   return answers
    /// ]]></code>
    /// 
    /// Figure 9.6 A simple backward-chaining algorithm.
    /// </summary>
    public class FOLBCAsk : IInferenceProcedure 
    {

        public IInferenceResult Ask(FOLKnowledgeBase kb, ISentence query) 
        {
            // Assertions on the type queries this Inference procedure
            // supports
            if (!(query is IAtomicSentence)) 
            {
                throw new ArgumentOutOfRangeException("query", "Only Atomic Queries are supported.");
            }

            IList<Literal> goals = new List<Literal>();
            goals.Add(new Literal((IAtomicSentence) query));

            var ansHandler = new BCAskAnswerHandler();

            IList<IList<ProofStepBwChGoal>> allProofSteps = this.Folbcask(kb, ansHandler,
                    goals, new Dictionary<Variable, ITerm>());

            ansHandler.SetAllProofSteps(allProofSteps);

            return ansHandler;
        }


        /// <summary>
        /// <code>
        /// function FOL-BC-ASK(KB, goals, theta) returns a set of substitutions
        ///   input: KB, a knowledge base
        ///          goals, a list of conjuncts forming a query (theta already applied)
        ///          theta, the current substitution, initially the empty substitution {}
        /// </code>
        /// </summary>
        /// <param name="KB"></param>
        /// <param name="ansHandler"></param>
        /// <param name="goals"></param>
        /// <param name="theta"></param>
        /// <returns></returns>
        private IList<IList<ProofStepBwChGoal>> Folbcask(FOLKnowledgeBase KB,
                BCAskAnswerHandler ansHandler, IList<Literal> goals,
                IDictionary<Variable, ITerm> theta) {
            var thisLevelProofSteps = new List<IList<ProofStepBwChGoal>>();
            // local variables: answers, a set of substitutions, initially empty

            // if goals is empty then return {theta}
            if (goals.Count == 0) {
                thisLevelProofSteps.Add(new List<ProofStepBwChGoal>());
                return thisLevelProofSteps;
            }

            // qDelta <- SUBST(theta, FIRST(goals))
            Literal qDelta = KB.Subst(theta, goals[0]);

            // for each sentence r in KB where
            // STANDARDIZE-APART(r) = (p1 ^ ... ^ pn => q)
            foreach (Clause r in KB.GetAllDefiniteClauses()) {
                var standardizedR = KB.StandardizeApart(r);
                // and thetaDelta <- UNIFY(q, qDelta) succeeds
                IDictionary<Variable, ITerm> thetaDelta = KB.Unify(standardizedR.GetPositiveLiterals()[0].AtomicSentence, qDelta.AtomicSentence);
                if (null != thetaDelta) {
                    // new_goals <- [p1,...,pn|REST(goals)]
                    var newGoals = new List<Literal>(standardizedR.GetNegativeLiterals());
                    newGoals.AddRange(goals.Skip(1));
                    // answers <- FOL-BC-ASK(KB, new_goals, COMPOSE(thetaDelta,
                    // theta)) U answers
                    IDictionary<Variable, ITerm> composed = this.Compose(KB, thetaDelta, theta);
                    IList<IList<ProofStepBwChGoal>> lowerLevelProofSteps = this.Folbcask(
                            KB, ansHandler, newGoals, composed);

                    ansHandler.AddProofStep(lowerLevelProofSteps, standardizedR, qDelta,
                            composed);

                    thisLevelProofSteps.AddRange(lowerLevelProofSteps);
                }
            }

            // return answers
            return thisLevelProofSteps;
        }

        // Artificial Intelligence A Modern Approach (2nd Edition): page 288.
        // COMPOSE(delta, tau) is the substitution whose effect is identical to
        // the effect of applying each substitution in turn. That is,
        // SUBST(COMPOSE(theta1, theta2), p) = SUBST(theta2, SUBST(theta1, p))
        private IDictionary<Variable, ITerm> Compose(FOLKnowledgeBase KB,
                IDictionary<Variable, ITerm> theta1, IDictionary<Variable, ITerm> theta2) {
            IDictionary<Variable, ITerm> composed = new Dictionary<Variable, ITerm>();

            // So that it behaves like:
            // SUBST(theta2, SUBST(theta1, p))
            // There are two steps involved here.
            // See: http://logic.stanford.edu/classes/cs157/2008/notes/chap09.pdf
            // for a detailed discussion:

            // 1. Apply theta2 to the range of theta1.
            foreach (Variable v in theta1.Keys) {
                composed[v] = KB.Subst(theta2, theta1[v]);
            }

            // 2. Adjoin to delta all pairs from tau with different
            // domain variables.
            foreach (Variable v in theta2.Keys) {
                if (!theta1.ContainsKey(v)) {
                    composed[v] = theta2[v];
                }
            }

            return this.CascadeSubstitutions(KB, composed);
        }

        // See:
        // http://logic.stanford.edu/classes/cs157/2008/miscellaneous/faq.html#jump165
        // for need for this.
        private IDictionary<Variable, ITerm> CascadeSubstitutions(FOLKnowledgeBase kb,
                IDictionary<Variable, ITerm> theta) {
            foreach (Variable v in theta.Keys)
            {
                ITerm t = theta[v];
                theta[v] = kb.Subst(theta, t);
            }

            return theta;
        }

        class BCAskAnswerHandler : IInferenceResult {

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

            // END-InferenceResult
            //

            public void SetAllProofSteps(IList<IList<ProofStepBwChGoal>> allProofSteps) {
                foreach (IList<ProofStepBwChGoal> steps in allProofSteps) {
                    ProofStepBwChGoal lastStep = steps[steps.Count - 1];
                    IDictionary<Variable, ITerm> theta = lastStep.Bindings;
                    proofs.Add(new ProofFinal(lastStep, theta));
                }
            }

            public void AddProofStep(
                    IList<IList<ProofStepBwChGoal>> currentLevelProofSteps,
                    Clause toProve, Literal currentGoal,
                    IDictionary<Variable, ITerm> bindings) {

                if (currentLevelProofSteps.Count > 0) {
                    ProofStepBwChGoal predecessor = new ProofStepBwChGoal(toProve,
                            currentGoal, bindings);
                    foreach (IList<ProofStepBwChGoal> steps in currentLevelProofSteps) {
                        if (steps.Count > 0) {
                            steps[0].SetPredecessor(predecessor);
                        }
                        steps.Insert(0, predecessor);
                    }
                }
            }
        }
    }

}
