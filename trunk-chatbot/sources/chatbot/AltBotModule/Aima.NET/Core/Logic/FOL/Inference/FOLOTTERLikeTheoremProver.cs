using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Inference.OTTER;
    using Aima.Core.Logic.FOL.Inference.OTTER.DefaultImpl;
    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (2nd Edition): Figure 9.14, page 307.
    /// 
    /// <code><![CDATA[
    /// procedure OTTER(sos, usable)
    ///   inputs: sos, a set of support-clauses defining the problem (a global variable)
    ///   usable, background knowledge potentially relevant to the problem
    ///   
    ///   repeat
    ///      clause <- the lightest member of sos
    ///      move clause from sos to usable
    ///      PROCESS(INFER(clause, usable), sos)
    ///   until sos = [] or a refutation has been found
    /// 
    /// --------------------------------------------------------------------------------
    /// 
    /// function INFER(clause, usable) returns clauses
    ///   
    ///   resolve clause with each member of usable
    ///   return the resulting clauses after applying filter
    ///   
    /// --------------------------------------------------------------------------------
    /// 
    /// procedure PROCESS(clauses, sos)
    /// 
    ///   for each clause in clauses do
    ///       clause <- SIMPLIFY(clause)
    ///       merge identical literals
    ///       discard clause if it is a tautology
    ///       sos <- [clause | sos]
    ///       if clause has no literals then a refutation has been found
    ///       if clause has one literal then look for unit refutation
    /// ]]></code>
    /// 
    /// Figure 9.14 Sketch of the OTTER theorem prover. Heuristic control is applied in the
    /// selection of the "lightest" clause and in the FILTER function that eliminates uninteresting
    /// clauses from consideration.
    /// TODO: look at implementing theorem below 
    /// Note: The original implementation of OTTER has been retired 
    ///  but its successor, Prover9, can be found at:
    ///  http:// www.prover9.org/
    ///  or
    ///  http://www.cs.unm.edu/~mccune/mace4/
    ///  Should you wish to play with a mature implementation of a theorem prover :-)
    ///  For lots of interesting problems to play with, see
    ///  'The TPTP Problem Library for Automated Theorem Proving':
    ///  http://www.cs.miami.edu/~tptp/
    /// </summary>
    public class FOLOTTERLikeTheoremProver : IInferenceProcedure 
    {
        //
        // Ten seconds is default maximum query time permitted
        public long MaxQueryTime { get; set; }

        public bool UseParamodulation { get; set; }

        public ILightestClauseHeuristic LightestClauseHeuristic { get; set; }

        public IClauseFilter ClauseFilter { get; set; }

        public IClauseSimplifier ClauseSimplifier { get; set; }
        //
        private Paramodulation paramodulation = new Paramodulation();

        public FOLOTTERLikeTheoremProver() : this(10* 1000, true)
        {
        }

        public FOLOTTERLikeTheoremProver(long maxQueryTime) :this(maxQueryTime, true)
        {
        }

        public FOLOTTERLikeTheoremProver(bool useParamodulation) : this(10 * 1000, useParamodulation)
        {
        }

        public FOLOTTERLikeTheoremProver(long maxQueryTime, bool useParamodulation) 
        {
            this.MaxQueryTime = maxQueryTime;
            this.UseParamodulation = useParamodulation;
            this.LightestClauseHeuristic = new DefaultLightestClauseHeuristic();
            this.ClauseFilter = new DefaultClauseFilter();
            this.ClauseSimplifier = new DefaultClauseSimplifier();
        }


        public IInferenceResult Ask(FOLKnowledgeBase KB, ISentence alpha) 
        {
            ISet<Clause> sos = new HashedSet<Clause>();
            ISet<Clause> usable = new HashedSet<Clause>();

            // Usable set will be the set of clauses in the KB,
            // are assuming this is satisfiable as using the
            // ISet of Support strategy.
            //foreach (var standardizedC in KB.GetAllClauses().Select(c => KB.StandardizeApart(c)))
            foreach (var standardizedC in KB.GetAllClauses())
            {
                standardizedC.SetStandardizedApartCheckNotRequired();
                usable.UnionWith(standardizedC.GetFactors());
            }

            // Ensure reflexivity axiom is added to usable if using paramodulation.
            if (this.UseParamodulation) 
            {
                // Reflexivity Axiom: x = x
                var reflexivityAxiom = new TermEquality(new Variable("x"), new Variable("x"));
                var reflexivityClause = new Clause();
                reflexivityClause.AddLiteral(new Literal(reflexivityAxiom));
                reflexivityClause = KB.StandardizeApart(reflexivityClause);
                reflexivityClause.SetStandardizedApartCheckNotRequired();
                usable.Add(reflexivityClause);
            }

            ISentence notAlpha = new NotSentence(alpha);
            // Want to use an answer literal to pull
            // query variables where necessary
            var answerLiteral = KB.CreateAnswerLiteral(notAlpha);
            var answerLiteralVariables = KB
                    .CollectAllVariables(answerLiteral.AtomicSentence);
            var answerClause = new Clause();

            if (answerLiteralVariables.Count > 0) {
                ISentence notAlphaWithAnswer = new ConnectedSentence(Connectors.Or,
                        notAlpha, answerLiteral.AtomicSentence);
               // foreach (var standardizedC in
              //      KB.ConvertToClauses(notAlphaWithAnswer).Select(KB.StandardizeApart))
                  foreach (var standardizedC in
                      KB.ConvertToClauses(notAlphaWithAnswer))
                   {
                       Clause c = KB.StandardizeApart(standardizedC);
                       c.SetProofStep(new ProofStepGoal(c));
                       c.SetStandardizedApartCheckNotRequired();
                       sos.UnionWith(c.GetFactors());
                   }

                answerClause.AddLiteral(answerLiteral);
            } 
            else 
            {
                //foreach (var standardizedC in
                //    KB.ConvertToClauses(notAlpha).Select(KB.StandardizeApart)) 
                foreach (var standardizedC in
                    KB.ConvertToClauses(notAlpha))
                {
                    Clause c = KB.StandardizeApart(standardizedC);
                    c.SetProofStep(new ProofStepGoal(c));
                    c.SetStandardizedApartCheckNotRequired();
                    sos.UnionWith(standardizedC.GetFactors());
                }
            }

            // Ensure all subsumed clauses are removed
            usable.ExceptWith(SubsumptionElimination.FindSubsumedClauses(usable));
            sos.ExceptWith(SubsumptionElimination.FindSubsumedClauses(sos));

            var ansHandler = new OTTERAnswerHandler(answerLiteral,
                    answerLiteralVariables, answerClause, this.MaxQueryTime);

            var idxdClauses = new IndexedClauses(LightestClauseHeuristic, sos, usable);

            return this.Otter(ansHandler, idxdClauses, sos, usable);
        }

        /// <summary>
        ///<code>
        /// procedure OTTER(sos, usable) 
        ///  inputs: sos, a set of support-clauses defining the problem (a global variable) 
        ///  usable, background knowledge potentially relevant to the problem
        ///</code>
        /// </summary>
        /// <param name="ansHandler"></param>
        /// <param name="idxdClauses"></param>
        /// <param name="sos"></param>
        /// <param name="usable"></param>
        /// <returns></returns>
        private IInferenceResult Otter(OTTERAnswerHandler ansHandler,
                IndexedClauses idxdClauses, ISet<Clause> sos, ISet<Clause> usable) 
        {

            LightestClauseHeuristic.InitialSOS(sos);

            // * repeat
            do {
                // * clause <- the lightest member of sos
                Clause clause = LightestClauseHeuristic.GetLightestClause();
                if (null != clause) 
                {
                    // * move clause from sos to usable
                    sos.Remove(clause);
                    LightestClauseHeuristic.RemovedClauseFromSOS(clause);
                    usable.Add(clause);
                    // * PROCESS(INFER(clause, usable), sos)
                    this.Process(ansHandler, idxdClauses, this.Infer(clause, usable), sos, usable);
                }

                // * until sos = [] or a refutation has been found
            } 
            while (sos.Count != 0 && !ansHandler.IsComplete());

            return ansHandler;
        }

        /// <summary>
        /// <code> function INFER(clause, usable) returns clauses </code>
        /// </summary>
        /// <param name="clause"></param>
        /// <param name="usable"></param>
        /// <returns></returns>
        private ISet<Clause> Infer(Clause clause, ISet<Clause> usable) 
        {
            ISet<Clause> resultingClauses = new HashedSet<Clause>();

            // * resolve clause with each member of usable
            foreach (var c in usable) 
            {
                ISet<Clause> resolvents = clause.BinaryResolvents(c);
                foreach (Clause rc in resolvents) 
                {
                    resultingClauses.Add(rc);
                }

                // if using paramodulation to handle equality
                if (this.UseParamodulation) 
                {
                    var paras = paramodulation.Apply(clause, c, true);
                    foreach (var p in paras) 
                    {
                        resultingClauses.Add(p);
                    }
                }
            }

            // * return the resulting clauses after applying filter
            return ClauseFilter.Filter(resultingClauses);
        }

        // procedure PROCESS(clauses, sos)
        private void Process(OTTERAnswerHandler ansHandler,
                IndexedClauses idxdClauses, ISet<Clause> clauses, ISet<Clause> sos,
                ISet<Clause> usable) {

            // * for each clause in clauses do
            foreach (Clause clause in clauses) 
            {
                // * clause <- SIMPLIFY(clause)
                var simplifiedClause = ClauseSimplifier.Simplify(clause);

                // * merge identical literals
                // Note: Not required as handled by Clause Implementation
                // which keeps literals within a ISet, so no duplicates
                // will exist.

                // * discard clause if it is a tautology
                if (simplifiedClause.IsTautology()) 
                {
                    continue;
                }

                // * if clause has no literals then a refutation has been found
                // or if it just Contains the answer literal.
                if (!ansHandler.IsAnswer(simplifiedClause)) 
                {
                    // * sos <- [clause | sos]
                    // This check ensure duplicate clauses are not
                    // introduced which will cause the
                    // ILightestClauseHeuristic to loop continuously
                    // on the same pair of objects.
                    if (!sos.Contains(simplifiedClause) && !usable.Contains(simplifiedClause)) 
                    {
                        foreach (var ac in
                            simplifiedClause.GetFactors().Where(ac => !sos.Contains(ac) && !usable.Contains(ac)))
                        {
                            idxdClauses.addClause(ac, sos, usable);

                            // * if clause has one literal then look for unit
                            // refutation
                            this.LookForUnitRefutation(ansHandler, idxdClauses, ac,
                                sos, usable);
                        }
                    }
                }

                if (ansHandler.IsComplete()) 
                {
                    break;
                }
            }
        }

        private void LookForUnitRefutation(OTTERAnswerHandler ansHandler,
                IndexedClauses idxdClauses, Clause clause, ISet<Clause> sos,
                ISet<Clause> usable) 
        {

            ISet<Clause> toCheck = new HashedSet<Clause>();

            if (ansHandler.IsCheckForUnitRefutation(clause)) 
            {
                foreach (Clause s in sos) 
                {
                    if (s.IsUnitClause()) 
                    {
                        toCheck.Add(s);
                    }
                }
                foreach (Clause u in usable) 
                {
                    if (u.IsUnitClause()) 
                    {
                        toCheck.Add(u);
                    }
                }
            }

            if (toCheck.Count > 0) 
            {
                toCheck = this.Infer(clause, toCheck);
                foreach (Clause t in toCheck) 
                {
                    // * clause <- SIMPLIFY(clause)
                    var simplifiedT = ClauseSimplifier.Simplify(t);

                    // * discard clause if it is a tautology
                    if (simplifiedT.IsTautology()) 
                    {
                        continue;
                    }

                    // * if clause has no literals then a refutation has been found
                    // or if it just Contains the answer literal.
                    if (!ansHandler.IsAnswer(simplifiedT)) 
                    {
                        // * sos <- [clause | sos]
                        // This check ensure duplicate clauses are not
                        // introduced which will cause the
                        // ILightestClauseHeuristic to loop continuously
                        // on the same pair of objects.
                        if (!sos.Contains(simplifiedT) && !usable.Contains(simplifiedT))
                        {
                            idxdClauses.addClause(simplifiedT, sos, usable);
                        }
                    }

                    if (ansHandler.IsComplete())
                    {
                        break;
                    }
                }
            }
        }

        // This is a simple indexing on the clauses to support
        // more efficient forward and backward subsumption testing.
        class IndexedClauses {
            private ILightestClauseHeuristic lightestClauseHeuristic = null;
            // Group the clauses by their # of literals.
            private IDictionary<int, ISet<Clause>> clausesGroupedBySize = new Dictionary<int, ISet<Clause>>();
            // Keep track of the min and max # of literals.
            private int minNoLiterals = int.MaxValue;
            private int maxNoLiterals = 0;

            public IndexedClauses(ILightestClauseHeuristic lightestClauseHeuristic,
                    ISet<Clause> sos, ISet<Clause> usable) {
                this.lightestClauseHeuristic = lightestClauseHeuristic;
                foreach (Clause c in sos) {
                    this.IndexClause(c);
                }
                foreach (Clause c in usable) {
                    this.IndexClause(c);
                }
            }

            public void addClause(Clause c, ISet<Clause> sos, ISet<Clause> usable)
            {
                // Perform forward subsumption elimination
                bool addToSOS = true;
                for (int i = this.minNoLiterals; i < c.GetNumberLiterals(); i++)
                {
                    var fs = this.clausesGroupedBySize[i];
                    if (null != fs)
                    {
                        addToSOS = fs.All(s => !s.Subsumes(c));
                    }
                    if (!addToSOS)
                    {
                        break;
                    }
                }

                if (addToSOS)
                {
                    sos.Add(c);
                    lightestClauseHeuristic.AddedClauseToSOS(c);
                    this.IndexClause(c);
                    // Have added clause, therefore
                    // perform backward subsumption elimination
                    ISet<Clause> subsumed = new HashedSet<Clause>();
                    for (var i = c.GetNumberLiterals() + 1; i <= this.maxNoLiterals; i++)
                    {
                        subsumed.Clear();
                        var bs = this.clausesGroupedBySize[i];
                        if (null == bs)
                        {
                            continue;
                        }

                        foreach (var s in bs.Where(c.Subsumes))
                        {
                            subsumed.Add(s);
                            if (sos.Contains(s))
                            {
                                sos.Remove(s);
                                this.lightestClauseHeuristic.RemovedClauseFromSOS(s);
                            }

                            usable.Remove(s);
                        }
                        bs.ExceptWith(subsumed);
                    }
                }
            }

            private void IndexClause(Clause c)
            {
                int size = c.GetNumberLiterals();
                if (size < this.minNoLiterals)
                {
                    this.minNoLiterals = size;
                }
                if (size > this.maxNoLiterals)
                {
                    this.maxNoLiterals = size;
                }
                
                var cforsize = this.clausesGroupedBySize[size];
                if (null == cforsize)
                {
                    cforsize = new HashedSet<Clause>();
                    this.clausesGroupedBySize[size] = cforsize;
                }

                cforsize.Add(c);
            }
        }

        class OTTERAnswerHandler : IInferenceResult 
        {
            private Literal answerLiteral = null;
            private ISet<Variable> answerLiteralVariables = null;
            private Clause answerClause = null;
            private long finishTime = 0L;
            private bool complete = false;
            private IList<IProof> proofs = new List<IProof>();
            private bool timedOut = false;

            public OTTERAnswerHandler(Literal answerLiteral,
                    ISet<Variable> answerLiteralVariables, Clause answerClause,
                    long maxQueryTime) 
            {
                this.answerLiteral = answerLiteral;
                this.answerLiteralVariables = answerLiteralVariables;
                this.answerClause = answerClause;

                var ts = DateTime.Now - DateTime.MinValue;
                this.finishTime = (long) ts.TotalMilliseconds + maxQueryTime;
            }

            public bool IsPossiblyFalse() 
            {
                return !timedOut && proofs.Count == 0;
            }

            public bool IsTrue() 
            {
                return proofs.Count > 0;
            }

            public bool IsUnknownDueToTimeout() 
            {
                return timedOut && proofs.Count == 0;
            }

            public bool IsPartialResultDueToTimeout() 
            {
                return timedOut && proofs.Count > 0;
            }

            public IList<IProof> GetProofs() 
            {
                return proofs;
            }

            public bool IsComplete() 
            {
                return complete;
            }

            public bool IsLookingForAnswerLiteral() 
            {
                return !answerClause.IsEmpty();
            }

            public bool IsCheckForUnitRefutation(Clause clause) 
            {

                if (this.IsLookingForAnswerLiteral()) 
                {
                    if (2 == clause.GetNumberLiterals()) 
                    {
                        return clause.GetLiterals().Any(t => t.AtomicSentence.GetSymbolicName().Equals(this.answerLiteral.AtomicSentence.GetSymbolicName()));
                    }
                } else {
                    return clause.IsUnitClause();
                }

                return false;
            }

            public bool IsAnswer(Clause aClause) 
            {
                bool isAns = false;

                if (answerClause.IsEmpty())
                {
                    if (aClause.IsEmpty()) 
                    {
                        proofs.Add(new ProofFinal(aClause.GetProofStep(), new Dictionary<Variable, ITerm>()));
                        complete = true;
                        isAns = true;
                    }
                } else 
                {
                    if (aClause.IsEmpty()) 
                    {
                        // This should not happen
                        // as added an answer literal to sos, which
                        // implies the database (i.e. premises) are
                        // unsatisfiable to begin with.
                        throw new InvalidOperationException(
                                "Generated an empty clause while looking for an answer, implies original KB or usable is unsatisfiable");
                    }

                    if (aClause.IsUnitClause()
                            && aClause.IsDefiniteClause()
                            && aClause.GetPositiveLiterals()[0].AtomicSentence.GetSymbolicName().Equals(
                                            this.answerLiteral.AtomicSentence.GetSymbolicName())
                        )
                    {
                        IDictionary<Variable, ITerm> answerBindings = new Dictionary<Variable, ITerm>();
                        IList<ITerm> answerTerms =
                            aClause.GetPositiveLiterals()[0].AtomicSentence.GetArgs().Cast<ITerm>().ToList();
                        var idx = 0;
                        foreach (Variable v in this.answerLiteralVariables)
                        {
                            answerBindings[v] = answerTerms[idx];
                            idx++;
                        }
                        bool addNewAnswer = this.proofs.All(p => !p.GetAnswerBindings().Equals(answerBindings));
                        if (addNewAnswer)
                        {
                            this.proofs.Add(new ProofFinal(aClause.GetProofStep(), answerBindings));
                        }
                        isAns = true;
                    }
                }

                var ts = DateTime.Now - DateTime.MinValue;
                if (ts.TotalMilliseconds > finishTime) 
                {
                    this.complete = true;

                    // Indicate that I have run out of query time
                    this.timedOut = true;
                }

                return isAns;
            }

            public override string ToString() 
            {
                StringBuilder sb = new StringBuilder();
                sb.Append("isComplete=" + complete);
                sb.Append("\n");
                sb.Append("result=" + proofs);
                return sb.ToString();
            }
        }
    }

}
