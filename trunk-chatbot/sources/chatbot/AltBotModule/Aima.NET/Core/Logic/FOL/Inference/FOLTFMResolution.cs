using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.Inference.Trace;
    using Aima.Core.Logic.FOL.KB;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 347.
    /// 
    /// The algorithmic approach is identical to the propositional case, described
    /// in Figure 7.12. 
    /// 
    /// However, this implementation will use the T)wo F)inger M)ethod 
    /// for looking for resolvents between clauses, which is very inefficient.
    ///  
    /// see: 
    /// http://logic.stanford.edu/classes/cs157/2008/lectures/lecture04.pdf,
    /// slide 21 for the propositional case.  
    /// In addition, an Answer literal will be used so that queries with Variables 
    /// may be answered (see pg. 350 of AIMA3e).
    /// </summary>
    public class FOLTFMResolution : IInferenceProcedure {

        private long maxQueryTime = 10 * 1000;

        public IFOLTFMResolutionTracer Tracer { get; set; }

        public FOLTFMResolution() {

        }

        public FOLTFMResolution(long maxQueryTime) {
            MaxQueryTime = maxQueryTime;
        }

        public FOLTFMResolution(IFOLTFMResolutionTracer tracer) {
            Tracer = tracer;
        }
        

        public long MaxQueryTime
        {
            get
            {
                return this.maxQueryTime;
            }
            set
            {
                this.maxQueryTime = value;
            }
        } 

        public IInferenceResult Ask(FOLKnowledgeBase kb, ISentence alpha) {

            // clauses <- the set of clauses in CNF representation of KB ^ ~alpha
            ISet<Clause> clauses = new HashedSet<Clause>();
            foreach (Clause c in kb.GetAllClauses()) {
                var standardizedC = kb.StandardizeApart(c);
                standardizedC.SetStandardizedApartCheckNotRequired();
                clauses.UnionWith(standardizedC.GetFactors());
            }
            ISentence notAlpha = new NotSentence(alpha);
            // Want to use an answer literal to pull
            // query variables where necessary
            Literal answerLiteral = kb.CreateAnswerLiteral(notAlpha);
            ISet<Variable> answerLiteralVariables = kb
                    .CollectAllVariables(answerLiteral.AtomicSentence);
            Clause answerClause = new Clause();

            if (answerLiteralVariables.Count > 0) {
                ISentence notAlphaWithAnswer = new ConnectedSentence(Connectors.Or,
                        notAlpha, answerLiteral.AtomicSentence);
                foreach (Clause c in kb.ConvertToClauses(notAlphaWithAnswer)) {
                    var standardizedC = kb.StandardizeApart(c);
                    standardizedC.SetProofStep(new ProofStepGoal(standardizedC));
                    standardizedC.SetStandardizedApartCheckNotRequired();
                    clauses.UnionWith(standardizedC.GetFactors());
                }

                answerClause.AddLiteral(answerLiteral);
            } else {
                foreach (Clause c in kb.ConvertToClauses(notAlpha)) {
                    var standardizedC = kb.StandardizeApart(c);
                    standardizedC.SetProofStep(new ProofStepGoal(standardizedC));
                    standardizedC.SetStandardizedApartCheckNotRequired();
                    clauses.UnionWith(standardizedC.GetFactors());
                }
            }

            var ansHandler = new TFMAnswerHandler(answerLiteral,
                    answerLiteralVariables, answerClause, this.MaxQueryTime);

            // new <- {}
            ISet<Clause> newClauses = new HashedSet<Clause>();
            ISet<Clause> toAdd = new HashedSet<Clause>();
            // loop do
            int noOfPrevClauses = clauses.Count;
            do {
                if (Tracer != null) 
                {
                    Tracer.StepStartWhile(clauses, clauses.Count, newClauses
                            .Count);
                }

                newClauses.Clear();

                // for each Ci, Cj in clauses do
                Clause[] clausesA = new Clause[clauses.Count];
                clausesA = clauses.ToArray();
                // Basically, using the simple T)wo F)inger M)ethod here.
                for (int i = 0; i < clausesA.Length; i++) {
                    Clause cI = clausesA[i];
                    if (null != Tracer) {
                        Tracer.StepOuterFor(cI);
                    }
                    for (int j = i; j < clausesA.Length; j++) {
                        Clause cJ = clausesA[j];

                        if (null != Tracer) {
                            Tracer.StepInnerFor(cI, cJ);
                        }

                        // resolvent <- FOL-RESOLVE(Ci, Cj)
                        ISet<Clause> resolvents = cI.BinaryResolvents(cJ);

                        if (resolvents.Count > 0) {
                            toAdd.Clear();
                            // new <- new <UNION> resolvent
                            foreach (Clause rc in resolvents) {
                                toAdd.UnionWith(rc.GetFactors());
                            }

                            if (null != Tracer) {
                                Tracer.StepResolved(cI, cJ, toAdd);
                            }

                            ansHandler.CheckForPossibleAnswers(toAdd);

                            if (ansHandler.IsComplete()) {
                                break;
                            }

                            newClauses.UnionWith(toAdd);
                        }

                        if (ansHandler.IsComplete())
                        {
                            break;
                        }
                    }
                    if (ansHandler.IsComplete()) {
                        break;
                    }
                }

                noOfPrevClauses = clauses.Count;

                // clauses <- clauses <UNION> new
                clauses.UnionWith(newClauses);

                if (ansHandler.IsComplete()) {
                    break;
                }

                // if new is a <SUBSET> of clauses then finished
                // searching for an answer
                // (i.e. when they were added the # clauses
                // did not increase).
            } while (noOfPrevClauses < clauses.Count);

            if (null != Tracer) {
                Tracer.StepFinished(clauses, ansHandler);
            }

            return ansHandler;
        }

        class TFMAnswerHandler : IInferenceResult 
        {
            private Literal answerLiteral = null;
            private ISet<Variable> answerLiteralVariables = null;
            private Clause answerClause = null;
            private long finishTime = 0L;
            private bool complete = false;
            private IList<IProof> proofs = new List<IProof>();
            private bool timedOut = false;

            public TFMAnswerHandler(Literal answerLiteral,
                    ISet<Variable> answerLiteralVariables, Clause answerClause,
                    long maxQueryTime) {
                this.answerLiteral = answerLiteral;
                this.answerLiteralVariables = answerLiteralVariables;
                this.answerClause = answerClause;
                //
                var ts = DateTime.Now - DateTime.MinValue;
                this.finishTime = (long) ts.TotalMilliseconds + maxQueryTime;
            }

            public bool IsPossiblyFalse() {
                return !timedOut && proofs.Count == 0;
            }

            public bool IsTrue() {
                return proofs.Count > 0;
            }

            public bool IsUnknownDueToTimeout() {
                return timedOut && proofs.Count == 0;
            }

            public bool IsPartialResultDueToTimeout() {
                return timedOut && proofs.Count > 0;
            }

            public IList<IProof> GetProofs() {
                return proofs;
            }

            public bool IsComplete() {
                return complete;
            }

            public void CheckForPossibleAnswers(ISet<Clause> resolvents) {
                // If no bindings being looked for, then
                // is just a true false query.
                foreach (Clause aClause in resolvents) {
                    if (answerClause.IsEmpty()) {
                        if (aClause.IsEmpty()) {
                            proofs.Add(new ProofFinal(aClause.GetProofStep(),
                                    new Dictionary<Variable, ITerm>()));
                            complete = true;
                        }
                    } else {
                        if (aClause.IsEmpty()) {
                            // This should not happen
                            // as added an answer literal, which
                            // implies the database (i.e. premises) are
                            // unsatisfiable to begin with.
                            throw new InvalidOperationException(
                                    "Generated an empty clause while looking for an answer, implies original KB is unsatisfiable");
                        }

                        if (aClause.IsUnitClause()
                                && aClause.IsDefiniteClause()
                                && aClause.GetPositiveLiterals()[0].AtomicSentence.GetSymbolicName()
                                        .Equals(answerLiteral.AtomicSentence.GetSymbolicName())) 
                        {
                            IDictionary<Variable, ITerm> answerBindings = new Dictionary<Variable, ITerm>();
                            IList<ITerm> answerTerms = aClause.GetPositiveLiterals()
                                    [0].AtomicSentence.GetArgs().Cast<ITerm>().ToList();
                            int idx = 0;
                            foreach (Variable v in answerLiteralVariables) {
                                answerBindings[v] = answerTerms[idx];
                                idx++;
                            }
                            bool addNewAnswer = true;
                            foreach (IProof p in proofs) {
                                if (p.GetAnswerBindings().Equals(answerBindings)) {
                                    addNewAnswer = false;
                                    break;
                                }
                            }
                            if (addNewAnswer) {
                                proofs.Add(new ProofFinal(aClause.GetProofStep(),
                                        answerBindings));
                            }
                        }
                    }

                    if ((DateTime.Now - DateTime.MinValue).TotalMilliseconds > finishTime) {
                        complete = true;
                        // Indicate that I have run out of query time
                        timedOut = true;
                    }
                }
            }

            public override string ToString() {
                StringBuilder sb = new StringBuilder();
                sb.Append("isComplete=" + complete);
                sb.Append("\n");
                sb.Append("result=" + proofs);
                return sb.ToString();
            }
        }
    }

}
