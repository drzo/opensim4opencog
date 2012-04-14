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
    /// Based on lecture notes from:
    /// http://logic.stanford.edu/classes/cs157/2008/lectures/lecture13.pdf
    /// </summary>
    public class FOLModelElimination : IInferenceProcedure 
    {
        // Ten seconds is default maximum query time permitted
        private long maxQueryTime = 10 * 1000;

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
        //
        private IFOLModelEliminationTracer tracer = null;
        //
        private Unifier unifier = new Unifier();
        private SubstVisitor substVisitor = new SubstVisitor();

        public FOLModelElimination() {

        }

        public FOLModelElimination(long maxQueryTime) {
            this.MaxQueryTime = maxQueryTime;
        }

        public FOLModelElimination(IFOLModelEliminationTracer tracer) {
            this.tracer = tracer;
        }

        public FOLModelElimination(IFOLModelEliminationTracer tracer,
                long maxQueryTime) {
            this.tracer = tracer;
            this.MaxQueryTime = maxQueryTime;
        }

        public IInferenceResult Ask(FOLKnowledgeBase kb, ISentence aQuery) {
            //
            // Get the background knowledge - are assuming this is satisfiable
            // as using Set of Support strategy.
            ISet<Clause> bgClauses = new HashedSet<Clause>(kb.GetAllClauses());
            bgClauses.ExceptWith(SubsumptionElimination.FindSubsumedClauses(bgClauses));
            IList<Chain> background = CreateChainsFromClauses(bgClauses);

            // Collect the information necessary for constructing
            // an answer (supports use of answer literals).
            var ansHandler = new AnswerHandler(kb, aQuery, this.MaxQueryTime);

            var ifps = new IndexedFarParents(ansHandler
                    .GetSetOfSupport(), background);

            // Iterative deepening to be used
            for (int maxDepth = 1; maxDepth < int.MaxValue; maxDepth++) {
                // Track the depth actually reached
                ansHandler.ResetMaxDepthReached();

                if (null != tracer) {
                    tracer.Reset();
                }

                foreach (Chain nearParent in ansHandler.GetSetOfSupport()) 
                {
                    this.RecursiveDls(maxDepth, 0, nearParent, ifps, ansHandler);
                    if (ansHandler.IsComplete()) {
                        return ansHandler;
                    }
                }
                // This means the search tree
                // has bottomed out (i.e. finite).
                // Return what I know based on exploring everything.
                if (ansHandler.GetMaxDepthReached() < maxDepth) {
                    return ansHandler;
                }
            }

            return ansHandler;
        }

        private static IList<Chain> CreateChainsFromClauses(ISet<Clause> clauses) 
        {
            var chains = new List<Chain>();

            foreach (Clause c in clauses) {
                Chain chn = new Chain(c.GetLiterals());
                chn.SetProofStep(new ProofStepChainFromClause(chn, c));
                chains.Add(chn);
                chains.AddRange(chn.GetContrapositives());
            }

            return chains;
        }

        // Recursive Depth Limited Search
        private void RecursiveDls(int maxDepth, int currentDepth, Chain nearParent,
                IndexedFarParents indexedFarParents, AnswerHandler ansHandler) {

            // Keep track of the maximum depth reached.
            ansHandler.UpdateMaxDepthReached(currentDepth);

            if (currentDepth == maxDepth) {
                return;
            }

            int noCandidateFarParents = indexedFarParents
                    .GetNumberCandidateFarParents(nearParent);
            if (null != tracer) {
                tracer.Increment(currentDepth, noCandidateFarParents);
            }
            indexedFarParents.StandardizeApart(nearParent);
            for (int farParentIdx = 0; farParentIdx < noCandidateFarParents; farParentIdx++) {
                // If have a complete answer, don't keep
                // checking candidate far parents
                if (ansHandler.IsComplete()) {
                    break;
                }

                // Reduction
                Chain nextNearParent = indexedFarParents.AttemptReduction(
                        nearParent, farParentIdx);

                if (null == nextNearParent) {
                    // Unable to remove the head via reduction
                    continue;
                }

                // Handle Canceling and Dropping
                bool cancelled;
                bool dropped;
                do {
                    cancelled = false;
                    Chain nextParent;
                    while (nextNearParent != (nextParent = this.TryCancellation(nextNearParent))) {
                        nextNearParent = nextParent;
                        cancelled = true;
                    }

                    dropped = false;
                    while (nextNearParent != (nextParent = this.TryDropping(nextNearParent))) {
                        nextNearParent = nextParent;
                        dropped = true;
                    }
                } while (dropped || cancelled);

                // Check if have answer before
                // going to the next level
                if (!ansHandler.IsAnswer(nextNearParent)) {
                    // Keep track of the current # of
                    // far parents that are possible for the next near parent.
                    int noNextFarParents = indexedFarParents
                            .GetNumberFarParents(nextNearParent);
                    // Add to indexed far parents
                    nextNearParent = indexedFarParents.AddToIndex(nextNearParent);

                    // Check the next level
                    this.RecursiveDls(maxDepth, currentDepth + 1, nextNearParent,
                            indexedFarParents, ansHandler);

                    // Reset the number of far parents possible
                    // when recursing back up.
                    indexedFarParents.ResetNumberFarParentsTo(nextNearParent,
                            noNextFarParents);
                }
            }
        }

        // Returns c if no cancellation occurred
        private Chain TryCancellation(Chain c) {
            Literal head = c.GetHead();
            if (null != head && !(head is ReducedLiteral)) {
                foreach (Literal l in c.GetTail()) {
                    if (l is ReducedLiteral) {
                        // if they can be resolved
                        if (head.IsNegativeLiteral() != l.IsNegativeLiteral()) {
                            IDictionary<Variable, ITerm> subst = unifier.Unify(head
                                    .AtomicSentence, l.AtomicSentence);
                            if (null != subst) {
                                // I have a cancellation
                                // Need to apply subst to all of the
                                // literals in the cancellation
                                IList<Literal> cancLits = new List<Literal>();
                                foreach (Literal lfc in c.GetTail()) {
                                    IAtomicSentence a = (IAtomicSentence) substVisitor
                                            .Subst(subst, lfc.AtomicSentence);
                                    cancLits.Add(lfc.NewInstance(a));
                                }
                                Chain cancellation = new Chain(cancLits);
                                cancellation
                                        .SetProofStep(new ProofStepChainCancellation(
                                                cancellation, c, subst));
                                return cancellation;
                            }
                        }
                    }
                }
            }
            return c;
        }

        // Returns c if no dropping occurred
        private Chain TryDropping(Chain c) {
            Literal head = c.GetHead();
            if (null != head && (head is ReducedLiteral)) {
                Chain dropped = new Chain(c.GetTail());
                dropped.SetProofStep(new ProofStepChainDropped(dropped, c));
                return dropped;
            }

            return c;
        }

        class AnswerHandler : IInferenceResult {
            private Chain answerChain = new Chain();
            private ISet<Variable> answerLiteralVariables;
            private IList<Chain> sos = null;
            private bool complete = false;
            private long finishTime = 0L;
            private int maxDepthReached = 0;
            private IList<IProof> proofs = new List<IProof>();
            private bool timedOut = false;

            public AnswerHandler(FOLKnowledgeBase kb, ISentence aQuery,
                    long maxQueryTime) {

                var ts = DateTime.Now - DateTime.MinValue;
                this.finishTime = (long) ts.TotalMilliseconds + maxQueryTime;

                ISentence refutationQuery = new NotSentence(aQuery);

                // Want to use an answer literal to pull
                // query variables where necessary
                Literal answerLiteral = kb.CreateAnswerLiteral(refutationQuery);
                answerLiteralVariables = kb.CollectAllVariables(answerLiteral
                        .AtomicSentence);

                // Create the Set of Support based on the Query.
                if (answerLiteralVariables.Count > 0) {
                    ISentence refutationQueryWithAnswer = new ConnectedSentence(
                            Connectors.Or, refutationQuery, (ISentence) answerLiteral.AtomicSentence.Copy());

                    sos = CreateChainsFromClauses(kb
                            .ConvertToClauses(refutationQueryWithAnswer));

                    answerChain.AddLiteral(answerLiteral);
                } else {
                    sos = CreateChainsFromClauses(kb
                            .ConvertToClauses(refutationQuery));
                }

                foreach (Chain s in sos) {
                    s.SetProofStep(new ProofStepGoal(s));
                }
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

            public IList<Chain> GetSetOfSupport() {
                return sos;
            }

            public bool IsComplete() {
                return complete;
            }

            public void ResetMaxDepthReached() {
                maxDepthReached = 0;
            }

            public int GetMaxDepthReached() {
                return maxDepthReached;
            }

            public void UpdateMaxDepthReached(int depth) {
                if (depth > maxDepthReached) {
                    maxDepthReached = depth;
                }
            }

            public bool IsAnswer(Chain nearParent) {
                bool isAns = false;
                if (answerChain.IsEmpty()) {
                    if (nearParent.IsEmpty()) {
                        proofs.Add(new ProofFinal(nearParent.ProofStep,
                                new Dictionary<Variable, ITerm>()));
                        complete = true;
                        isAns = true;
                    }
                } else {
                    if (nearParent.IsEmpty()) {
                        // This should not happen
                        // as added an answer literal to sos, which
                        // implies the database (i.e. premises) are
                        // unsatisfiable to begin with.
                        throw new InvalidOperationException(
                                "Generated an empty chain while looking for an answer, implies original KB is unsatisfiable");
                    }
                    if (1 == nearParent.GetNumberLiterals()
                            && nearParent.GetHead().AtomicSentence
                                    .GetSymbolicName().Equals(
                                            answerChain.GetHead()
                                                    .AtomicSentence
                                                    .GetSymbolicName())) {
                        IDictionary<Variable, ITerm> answerBindings = new Dictionary<Variable, ITerm>();
                        IList<ITerm> answerTerms = nearParent.GetHead()
                                .AtomicSentence.GetArgs().Cast<ITerm>().ToList();
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
                            proofs.Add(new ProofFinal(nearParent.ProofStep,
                                    answerBindings));
                        }
                        isAns = true;
                    }
                }

                if ((DateTime.Now - DateTime.MinValue).TotalMilliseconds > finishTime) {
                    complete = true;
                    // Indicate that I have run out of query time
                    timedOut = true;
                }

                return isAns;
            }

            public override string ToString() {
                var sb = new StringBuilder();
                sb.Append("isComplete=" + complete);
                sb.Append("\n");
                sb.Append("result=" + proofs);
                return sb.ToString();
            }
        }
    }

    class IndexedFarParents 
    {
        //
        private int saIdx = 0;
        private Unifier unifier = new Unifier();
        private SubstVisitor substVisitor = new SubstVisitor();
        //
        private IDictionary<string, IList<Chain>> posHeads = new Dictionary<string, IList<Chain>>();
        private IDictionary<string, IList<Chain>> negHeads = new Dictionary<string, IList<Chain>>();

        public IndexedFarParents(IList<Chain> sos, IList<Chain> background) {
            this.ConstructInternalDataStructures(sos, background);
        }

        public int GetNumberFarParents(Chain farParent) {
            Literal head = farParent.GetHead();

            IDictionary<string, IList<Chain>> heads = null;
            if (head.IsPositiveLiteral()) {
                heads = posHeads;
            } else {
                heads = negHeads;
            }
            string headKey = head.AtomicSentence.GetSymbolicName();

            IList<Chain> farParents = heads[headKey];
            if (null != farParents) {
                return farParents.Count;
            }
            return 0;
        }

        public void ResetNumberFarParentsTo(Chain farParent, int toSize) {
            Literal head = farParent.GetHead();
            IDictionary<string, IList<Chain>> heads = null;
            if (head.IsPositiveLiteral()) {
                heads = posHeads;
            } else {
                heads = negHeads;
            }
            string key = head.AtomicSentence.GetSymbolicName();
            IList<Chain> farParents = heads[key];
            while (farParents.Count > toSize) {
                farParents.RemoveAt(farParents.Count - 1);
            }
        }

        public int GetNumberCandidateFarParents(Chain nearParent) {
            Literal nearestHead = nearParent.GetHead();

            IDictionary<string, IList<Chain>> candidateHeads = null;
            if (nearestHead.IsPositiveLiteral()) {
                candidateHeads = negHeads;
            } else {
                candidateHeads = posHeads;
            }

            string nearestKey = nearestHead.AtomicSentence.GetSymbolicName();

            IList<Chain> farParents = candidateHeads[nearestKey];
            if (null != farParents) {
                return farParents.Count;
            }
            return 0;
        }

        public Chain AttemptReduction(Chain nearParent, int farParentIndex) {
            Chain nnpc = null;

            Literal nearLiteral = nearParent.GetHead();

            IDictionary<string, IList<Chain>> candidateHeads = null;
            if (nearLiteral.IsPositiveLiteral()) {
                candidateHeads = negHeads;
            } else {
                candidateHeads = posHeads;
            }

            IAtomicSentence nearAtom = nearLiteral.AtomicSentence;
            string nearestKey = nearAtom.GetSymbolicName();
            IList<Chain> farParents = candidateHeads[nearestKey];
            if (null != farParents) {
                Chain farParent = farParents[farParentIndex];
                StandardizeApart(farParent);
                Literal farLiteral = farParent.GetHead();
                IAtomicSentence farAtom = farLiteral.AtomicSentence;
                IDictionary<Variable, ITerm> subst = unifier.Unify(nearAtom, farAtom);

                // If I was able to Unify with one
                // of the far heads
                if (null != subst) {
                    // Want to always apply reduction uniformly
                    Chain topChain = farParent;
                    Literal botLit = nearLiteral;
                    Chain botChain = nearParent;

                    // Need to apply subst to all of the
                    // literals in the reduction
                    IList<Literal> reduction = new List<Literal>();
                    foreach (Literal l in topChain.GetTail()) {
                        IAtomicSentence atom = (IAtomicSentence) substVisitor.Subst(
                                subst, l.AtomicSentence);
                        reduction.Add(l.NewInstance(atom));
                    }
                    reduction.Add(new ReducedLiteral((IAtomicSentence) substVisitor
                            .Subst(subst, botLit.AtomicSentence), botLit
                            .IsNegativeLiteral()));
                    foreach (Literal l in botChain.GetTail()) {
                        IAtomicSentence atom = (IAtomicSentence) substVisitor.Subst(
                                subst, l.AtomicSentence);
                        reduction.Add(l.NewInstance(atom));
                    }

                    nnpc = new Chain(reduction);
                    nnpc.SetProofStep(new ProofStepChainReduction(nnpc, nearParent,
                            farParent, subst));
                }
            }

            return nnpc;
        }

        public Chain AddToIndex(Chain c) {
            Chain added = null;
            Literal head = c.GetHead();
            if (null != head) {
                IDictionary<string, IList<Chain>> toAddTo = null;
                if (head.IsPositiveLiteral()) {
                    toAddTo = posHeads;
                } else {
                    toAddTo = negHeads;
                }

                string key = head.AtomicSentence.GetSymbolicName();
                IList<Chain> farParents = toAddTo[key];
                if (null == farParents) {
                    farParents = new List<Chain>();
                    toAddTo[key] = farParents;
                }

                added = c;
                farParents.Add(added);
            }
            return added;
        }

        public void StandardizeApart(Chain c) {
            saIdx = StandardizeApartInPlace.StandardizeApart(c, saIdx);
        }

        public override string ToString() {
            StringBuilder sb = new StringBuilder();

            sb.Append("#");
            sb.Append(posHeads.Count);
            foreach (string key in posHeads.Keys) {
                sb.Append(",");
                sb.Append(posHeads[key].Count);
            }
            sb.Append(" posHeads=");
            sb.Append(posHeads.ToString());
            sb.Append("\n");
            sb.Append("#");
            sb.Append(negHeads.Count);
            foreach (string key in negHeads.Keys) {
                sb.Append(",");
                sb.Append(negHeads[key].Count);
            }
            sb.Append(" negHeads=");
            sb.Append(negHeads.ToString());

            return sb.ToString();
        }

        private void ConstructInternalDataStructures(IList<Chain> sos,
                IList<Chain> background)
        {
            var toIndex = new List<Chain>();
            toIndex.AddRange(sos);
            toIndex.AddRange(background);

            foreach (var c in toIndex)
            {
                this.AddToIndex(c);
            }
        }
    }
}
