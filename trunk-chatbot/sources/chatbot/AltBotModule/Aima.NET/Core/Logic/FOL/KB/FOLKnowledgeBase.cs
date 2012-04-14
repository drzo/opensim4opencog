using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.KB
{
    using System.Collections;
    using System.Collections.ObjectModel;
    using System.Runtime.CompilerServices;

    using Aima.Core.Logic.FOL.Domain;
    using Aima.Core.Logic.FOL.Inference;
    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    /// A First Order Logic (FOL) Knowledge Base.
    /// </summary>
    public class FOLKnowledgeBase 
    {
        private FOLParser parser;
        private IInferenceProcedure inferenceProcedure;
        private Unifier unifier;
        private SubstVisitor substVisitor;
        private VariableCollector variableCollector;
        private StandardizeApart standardizeApart;
        private CNFConverter cnfConverter;
        //
        // Persistent data structures
        //
        // Keeps track of the Sentences in their original form as added to the
        // Knowledge base.
        private IList<ISentence> originalSentences = new List<ISentence>();
        // The KB in clause form
        private ISet<Clause> clauses = new HashedSet<Clause>();
        // Keep track of all of the definite clauses in the database
        // along with those that represent implications.
        private IList<Clause> allDefiniteClauses = new List<Clause>();
        private IList<Clause> implicationDefiniteClauses = new List<Clause>();
        // All the facts in the KB indexed by Atomic Sentence name (Note: pg. 279)
        private IDictionary<string, IList<Literal>> indexFacts = new Dictionary<string, IList<Literal>>();
        // Keep track of indexical keys for uniquely standardizing apart sentences
        private IStandardizeApartIndexical variableIndexical = StandardizeApartIndexicalFactory
                .NewStandardizeApartIndexical('v');
        private IStandardizeApartIndexical queryIndexical = StandardizeApartIndexicalFactory
                .NewStandardizeApartIndexical('q');

        //
        // PUBLIC METHODS
        //
        public FOLKnowledgeBase(FOLDomain domain) : this(domain, new FOLOTTERLikeTheoremProver())
        {
        }

        public FOLKnowledgeBase(FOLDomain domain, IInferenceProcedure inferenceProcedure) : this(domain, inferenceProcedure, new Unifier())
        {
        }

        public FOLKnowledgeBase(FOLDomain domain,
                IInferenceProcedure inferenceProcedure, Unifier unifier) {
            this.parser = new FOLParser(new FOLDomain(domain));
            this.inferenceProcedure = inferenceProcedure;
            this.unifier = unifier;
            //
            this.substVisitor = new SubstVisitor();
            this.variableCollector = new VariableCollector();
            this.standardizeApart = new StandardizeApart(variableCollector,
                    substVisitor);
            this.cnfConverter = new CNFConverter(parser);
        }

        public void Clear() {
            this.originalSentences.Clear();
            this.clauses.Clear();
            this.allDefiniteClauses.Clear();
            this.implicationDefiniteClauses.Clear();
            this.indexFacts.Clear();
        }

        public IInferenceProcedure GetInferenceProcedure() {
            return inferenceProcedure;
        }

        public void SetInferenceProcedure(IInferenceProcedure inferenceProcedure) {
            if (null != inferenceProcedure) {
                this.inferenceProcedure = inferenceProcedure;
            }
        }

        public ISentence tell(string aSentence) {
            ISentence s = parser.Parse(aSentence);
            tell(s);
            return s;
        }

        public void tell(IList<ISentence> sentences) {
            foreach (ISentence s in sentences) {
                tell(s);
            }
        }

        public void tell(ISentence aSentence) {
            this.Store(aSentence);
        }

        /**
         * 
         * @param aQuerySentence
         * @return an IInferenceResult.
         */
        public IInferenceResult Ask(string aQuerySentence) {
            return Ask(parser.Parse(aQuerySentence));
        }

        public IInferenceResult Ask(ISentence aQuery) {
            // Want to standardize apart the query to ensure
            // it does not clash with any of the sentences
            // in the database
            StandardizeApartResult saResult = standardizeApart.GetStandardizeApartResult(
                    aQuery, queryIndexical);

            // Need to map the result variables (as they are standardized apart)
            // to the original queries variables so that the caller can easily
            // understand and use the returned set of substitutions
            IInferenceResult infResult = this.GetInferenceProcedure().Ask(this,
                    saResult.Standardized);
            foreach (IProof p in infResult.GetProofs()) 
{
                IDictionary<Variable, ITerm> im = p.GetAnswerBindings();
                IDictionary<Variable, ITerm> em = new Dictionary<Variable, ITerm>();
                foreach (Variable rev in saResult.ReverseSubstitution.Keys) 
{
                    em[(Variable) saResult.ReverseSubstitution[rev]] = im[rev];
                }
                p.ReplaceAnswerBindings(em);
            }

            return infResult;
        }

        public int GetNumberFacts() {
            return allDefiniteClauses.Count - implicationDefiniteClauses.Count;
        }

        public int GetNumberRules() {
            return clauses.Count - this.GetNumberFacts();
        }

        public IList<ISentence> GetOriginalSentences() {
            return new ReadOnlyCollection<ISentence>(originalSentences);
        }

        public IList<Clause> GetAllDefiniteClauses() {
            return new ReadOnlyCollection<Clause>(allDefiniteClauses);
        }

        public IList<Clause> GetAllDefiniteClauseImplications() {
            return new ReadOnlyCollection<Clause>(implicationDefiniteClauses);
        }

        public ISet<Clause> GetAllClauses() 
        {
            // TODO: add real ReadonlySet support
            return new HashedSet<Clause>(this.clauses);
        }

        // Note: pg 278, FETCH(q) concept.
        [MethodImpl(MethodImplOptions.Synchronized)]
        public ISet<IDictionary<Variable, ITerm>> Fetch(Literal l) 
        {
            // Get all of the substitutions in the KB that p unifies with
            ISet<IDictionary<Variable, ITerm>> allUnifiers = new HashedSet<IDictionary<Variable, ITerm>>();

            IList<Literal> matchingFacts = this.FetchMatchingFacts(l);
            if (null != matchingFacts) {
                foreach (Literal fact in matchingFacts) {
                    IDictionary<Variable, ITerm> substitution = unifier.Unify(l
                            .AtomicSentence, fact.AtomicSentence);
                    if (null != substitution) {
                        allUnifiers.Add(substitution);
                    }
                }
            }

            return allUnifiers;
        }

        // Note: To support FOL-FC-Ask
        public ISet<IDictionary<Variable, ITerm>> Fetch(IList<Literal> literals) {
            ISet<IDictionary<Variable, ITerm>> possibleSubstitutions = new HashedSet<IDictionary<Variable, ITerm>>();

            if (literals.Count > 0) {
                Literal first = literals[0];
                IList<Literal> rest = literals.Skip(1).ToList();

                this.RecursiveFetch(new Dictionary<Variable, ITerm>(), first, rest,
                        possibleSubstitutions);
            }

            return possibleSubstitutions;
        }

        public IDictionary<Variable, ITerm> Unify(IFOLNode x, IFOLNode y) {
            return unifier.Unify(x, y);
        }

        public ISentence Subst(IDictionary<Variable, ITerm> theta, ISentence aSentence) {
            return substVisitor.Subst(theta, aSentence);
        }

        public Literal Subst(IDictionary<Variable, ITerm> theta, Literal l) {
            return substVisitor.Subst(theta, l);
        }

        public ITerm Subst(IDictionary<Variable, ITerm> theta, ITerm aTerm) {
            return substVisitor.Subst(theta, aTerm);
        }

        // Note: see page 277.
        public ISentence StandardizeApart(ISentence aSentence) {
            return standardizeApart.GetStandardizeApartResult(aSentence, variableIndexical)
                    .Standardized;
        }

        public Clause StandardizeApart(Clause aClause) {
            return standardizeApart.GetStandardizeApartResult(aClause, variableIndexical);
        }

        public Chain StandardizeApart(Chain aChain) {
            return standardizeApart.GetStandardizeApartResult(aChain, variableIndexical);
        }

        public ISet<Variable> CollectAllVariables(ISentence aSentence) {
            return variableCollector.CollectAllVariables(aSentence);
        }

        public CNF ConvertToCNF(ISentence aSentence) {
            return cnfConverter.ConvertToCNF(aSentence);
        }

        public ISet<Clause> ConvertToClauses(ISentence aSentence) {
            CNF cnf = cnfConverter.ConvertToCNF(aSentence);

            return new HashedSet<Clause>(cnf.GetConjunctionOfClauses());
        }

        public Literal CreateAnswerLiteral(ISentence forQuery) {
            string alName = parser.GetFOLDomain().AddAnswerLiteral();
            IList<ITerm> terms = new List<ITerm>();

            ISet<Variable> vars = variableCollector.CollectAllVariables(forQuery);
            foreach (Variable v in vars) {
                // Ensure copies of the variables are used.
                terms.Add((ITerm) v.Copy());
            }

            return new Literal(new Predicate(alName, terms));
        }

        // Note: see pg. 281
        public bool IsRenaming(Literal l) {
            IList<Literal> possibleMatches = this.FetchMatchingFacts(l);
            if (null != possibleMatches) {
                return this.IsRenaming(l, possibleMatches);
            }

            return false;
        }

        // Note: see pg. 281
        public bool IsRenaming(Literal l, IList<Literal> possibleMatches) {

            foreach (Literal q in possibleMatches) {
                if (l.IsPositiveLiteral() != q.IsPositiveLiteral()) {
                    continue;
                }
                IDictionary<Variable, ITerm> subst = unifier.Unify(l.AtomicSentence, q
                        .AtomicSentence);
                if (null != subst) {
                    int cntVarTerms = 0;
                    foreach (ITerm t in subst.Values) {
                        if (t is Variable) {
                            cntVarTerms++;
                        }
                    }
                    // If all the substitutions, even if none, map to Variables
                    // then this is a renaming
                    if (subst.Count == cntVarTerms) {
                        return true;
                    }
                }
            }

            return false;
        }

        public override string ToString() {
            var sb = new StringBuilder();
            foreach (ISentence s in originalSentences) {
                sb.Append(s.ToString());
                sb.Append("\n");
            }
            return sb.ToString();
        }

        //
        // PROTECTED METHODS
        //

        protected FOLParser GetParser() {
            return parser;
        }

        //
        // PRIVATE METHODS
        //

        // Note: pg 278, STORE(s) concept.
        [MethodImpl(MethodImplOptions.Synchronized)]
        private void Store(ISentence aSentence) {
            originalSentences.Add(aSentence);

            // Convert the sentence to CNF
            CNF cnfOfOrig = cnfConverter.ConvertToCNF(aSentence);
            foreach (Clause c in cnfOfOrig.GetConjunctionOfClauses()) {
                c.SetProofStep(new ProofStepClauseClausifySentence(c, aSentence));
                if (c.IsEmpty()) {
                    // This should not happen, if so the user
                    // is trying to add an unsatisfiable sentence
                    // to the KB.
                    throw new InvalidOperationException(
                            "Attempted to add unsatisfiable sentence to KB, orig=["
                                    + aSentence + "] CNF=" + cnfOfOrig);
                }

                // Ensure all clauses added to the KB are Standardized Apart.
                var standardizedC = standardizeApart.GetStandardizeApartResult(c, variableIndexical);

                // Will make all clauses immutable
                // so that they cannot be modified externally.
                standardizedC.Immutable = true;
                if (clauses.Add(standardizedC)) {
                    // If added keep track of special types of
                    // clauses, as useful for query purposes
                    if (standardizedC.IsDefiniteClause()) {
                        allDefiniteClauses.Add(standardizedC);
                    }
                    if (standardizedC.IsImplicationDefiniteClause()) {
                        implicationDefiniteClauses.Add(standardizedC);
                    }
                    if (standardizedC.IsUnitClause()) {
                        this.IndexFact(standardizedC.GetLiterals().First());
                    }
                }
            }
        }

        // Only if it is a unit clause does it get indexed as a fact
        // see pg. 279 for general idea.
        private void IndexFact(Literal fact) {
            string factKey = this.GetFactKey(fact);
            if (!indexFacts.ContainsKey(factKey)) {
                indexFacts[factKey] = new List<Literal>();
            }

            indexFacts[factKey].Add(fact);
        }

        private void RecursiveFetch(IDictionary<Variable, ITerm> theta, Literal l,
                IList<Literal> remainingLiterals,
                ISet<IDictionary<Variable, ITerm>> possibleSubstitutions) {

            // Find all substitutions for current predicate based on the
            // substitutions of prior predicates in the list (i.e. SUBST with
            // theta).
            ISet<IDictionary<Variable, ITerm>> pSubsts = this.Fetch(Subst(theta, l));

            // No substitutions, therefore cannot continue
            if (null == pSubsts) 
            {
                return;
            }

            foreach (IDictionary<Variable, ITerm> psubst in pSubsts) {
                // Ensure all prior substitution information is maintained
                // along the chain of predicates (i.e. for shared variables
                // across the predicates).
                foreach(var thetaItem in theta)
                {
                    psubst.Add(thetaItem);
                }
                if (remainingLiterals.Count == 0) {
                    // This means I am at the end of the chain of predicates
                    // and have found a valid substitution.
                    possibleSubstitutions.Add(psubst);
                } else {
                    // Need to move to the next link in the chain of substitutions
                    Literal first = remainingLiterals[0];
                    IList<Literal> rest = remainingLiterals.Skip(1).ToList();

                    this.RecursiveFetch(psubst, first, rest, possibleSubstitutions);
                }
            }
        }

        private IList<Literal> FetchMatchingFacts(Literal l) {
            return indexFacts[this.GetFactKey(l)];
        }

        private string GetFactKey(Literal l)
        {
            StringBuilder key = new StringBuilder();
            if (l.IsPositiveLiteral())
            {
                key.Append("+");
            }
            else
            {
                key.Append("-");
            }

            key.Append(l.AtomicSentence.GetSymbolicName());

            return key.ToString();
        }
    }
}
