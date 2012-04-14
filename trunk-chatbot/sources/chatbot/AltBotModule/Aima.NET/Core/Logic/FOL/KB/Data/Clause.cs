using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.KB.Data
{
    using System.Collections.ObjectModel;
    using System.IO;

    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;
    using Aima.Core.Util.Math;

    /// <summary>
    /// A Clause: A disjunction of literals.
    /// </summary>
    public class Clause 
    {
        //
        private static IStandardizeApartIndexical _saIndexical = StandardizeApartIndexicalFactory
                .NewStandardizeApartIndexical('c');
        private static Unifier _unifier = new Unifier();
        private static SubstVisitor _substVisitor = new SubstVisitor();
        private static VariableCollector _variableCollector = new VariableCollector();
        private static StandardizeApart _standardizeApart = new StandardizeApart();
        private static LiteralsSorter _literalSorter = new LiteralsSorter();
        //
        private readonly ISet<Literal> literals = new HashedSet<Literal>();
        private readonly IList<Literal> positiveLiterals = new List<Literal>();
        private readonly IList<Literal> negativeLiterals = new List<Literal>();

        public bool Immutable { get; set; }
        private bool saCheckRequired = true;

        public string EqualityIdentity { get; private set; }
        private ISet<Clause> factors;
        private ISet<Clause> nonTrivialFactors;
        private string stringRep;
        private IProofStep proofStep;

        public Clause()
        {
            // i.e. the empty clause
        }

        public Clause(IList<Literal> lits) 
        {
            this.literals.UnionWith(lits);
            foreach (var l in literals) 
            {
                if (l.IsPositiveLiteral()) {
                    this.positiveLiterals.Add(l);
                } else {
                    this.negativeLiterals.Add(l);
                }
            }
            this.RecalculateIdentity();
        }

        public Clause(IList<Literal> lits1, IList<Literal> lits2)
        {
            literals.UnionWith(lits1);
            literals.UnionWith(lits2);
            foreach (var l in literals) 
            {
                if (l.IsPositiveLiteral()) 
                {
                    this.positiveLiterals.Add(l);
                } 
                else 
                {
                    this.negativeLiterals.Add(l);
                }
            }
            this.RecalculateIdentity();
        }

        public IProofStep GetProofStep() 
        {
            if (null == proofStep) 
            {
                // Assume was a premise
                proofStep = new ProofStepPremise(this);
            }
            return proofStep;
        }

        public void SetProofStep(IProofStep pStep) 
        {
            this.proofStep = pStep;
        }

        public bool IsStandardizedApartCheckRequired() 
        {
            return saCheckRequired;
        }

        public void SetStandardizedApartCheckNotRequired() 
        {
            saCheckRequired = false;
        }

        public bool IsEmpty() 
        {
            return literals.Count == 0;
        }

        public bool IsUnitClause() 
        {
            return literals.Count == 1;
        }

        public bool IsDefiniteClause() 
        {
            // A Definite Clause is a disjunction of literals of which exactly 1 is
            // positive.
            return !IsEmpty() && positiveLiterals.Count == 1;
        }

        public bool IsImplicationDefiniteClause() 
        {
            // An Implication Definite Clause is a disjunction of literals of
            // which exactly 1 is positive and there is 1 or more negative
            // literals.
            return IsDefiniteClause() && negativeLiterals.Count >= 1;
        }

        public bool IsHornClause() 
        {
            // A Horn clause is a disjunction of literals of which at most one is
            // positive.
            return !IsEmpty() && positiveLiterals.Count <= 1;
        }

        public bool IsTautology()
        {
            return (from pl in this.positiveLiterals
                    from nl in this.negativeLiterals
                    where pl.AtomicSentence.Equals(nl.AtomicSentence)
                    select pl).Any();
        }

        public void AddLiteral(Literal literal) 
        {
            if (this.Immutable) 
            {
                throw new InvalidDataException("Clause is immutable, cannot be updated.");
            }
            var origSize = literals.Count;
            literals.Add(literal);
            if (literals.Count > origSize) 
            {
                if (literal.IsPositiveLiteral()) 
                {
                    positiveLiterals.Add(literal);
                } else {
                    negativeLiterals.Add(literal);
                }
            }
            this.RecalculateIdentity();
        }

        public void AddPositiveLiteral(IAtomicSentence atom) 
        {
            this.AddLiteral(new Literal(atom));
        }

        public void AddNegativeLiteral(IAtomicSentence atom) 
        {
            this.AddLiteral(new Literal(atom, true));
        }

        public int GetNumberLiterals() 
        {
            return literals.Count;
        }

        public int GetNumberPositiveLiterals() 
        {
            return positiveLiterals.Count;
        }

        public int GetNumberNegativeLiterals() 
        {
            return negativeLiterals.Count;
        }

        public ISet<Literal> GetLiterals() 
        {
            //TODO: add readonly set logic
            return new HashedSet<Literal>(literals);
        }

        public IList<Literal> GetPositiveLiterals()
        {
            return new ReadOnlyCollection<Literal>(positiveLiterals);
        }

        public IList<Literal> GetNegativeLiterals()
        {
            return new ReadOnlyCollection<Literal>(negativeLiterals);
        }

        public ISet<Clause> GetFactors() 
        {
            if (this.factors == null) 
            {
                this.CalculateFactors(null);
            }
            //TODO: add readonly logic
            return new HashedSet<Clause>(factors);
        }

        public ISet<Clause> GetNonTrivialFactors() 
        {
            if (null == nonTrivialFactors) 
            {
                this.CalculateFactors(null);
            }
            //TODO: add readonly logic
            return new HashedSet<Clause>(nonTrivialFactors);
        }

        public bool Subsumes(Clause othC) 
        {
            var subsumes = false;

            // Equality is not subsumption
            if (this != othC) 
            {
                // Ensure this has less literals total and that
                // it is a subset of the other clauses positive and negative counts
                if (this.GetNumberLiterals() < othC.GetNumberLiterals()
                        && this.GetNumberPositiveLiterals() <= othC
                                .GetNumberPositiveLiterals()
                        && this.GetNumberNegativeLiterals() <= othC
                                .GetNumberNegativeLiterals()) {

                    var thisToTry = this.CollectLikeLiterals(this.literals);
                    var othCToTry = this.CollectLikeLiterals(othC.literals);
                    // Ensure all like literals from this clause are a subset
                    // of the other clause.
                    if (thisToTry.Keys.All(othCToTry.Keys.Contains)) 
                    {
                        // Ensure that each set of same named literals
                        // from this clause is a subset of the other
                        // clauses same named literals.
                        bool isAPossSubset = thisToTry.Keys.All(pk => thisToTry[pk].Count <= othCToTry[pk].Count);
                        if (isAPossSubset) {
                            // At this point I know this this Clause's
                            // literal/arity names are a subset of the
                            // other clauses literal/arity names
                            subsumes = this.CheckSubsumes(othC, thisToTry, othCToTry);
                        }
                    }
                }
            }

            return subsumes;
        }

        // Note: Applies binary resolution rule and factoring
        // Note: returns a set with an empty clause if both clauses
        // are empty, otherwise returns a set of binary resolvents.
        public ISet<Clause> BinaryResolvents(Clause othC) 
        {
            ISet<Clause> resolvents = new HashedSet<Clause>();
            // Resolving two empty clauses
            // gives you an empty clause
            if (IsEmpty() && othC.IsEmpty()) 
            {
                resolvents.Add(new Clause());
                return resolvents;
            }

            // Ensure Standardized Apart
            // Before attempting binary resolution
            othC = this.SaIfRequired(othC);

            var allPosLits = new List<Literal>();
            var allNegLits = new List<Literal>();
            allPosLits.AddRange(this.positiveLiterals);
            allPosLits.AddRange(othC.positiveLiterals);
            allNegLits.AddRange(this.negativeLiterals);
            allNegLits.AddRange(othC.negativeLiterals);

            var trPosLits = new List<Literal>();
            var trNegLits = new List<Literal>();
            var copyRPosLits = new List<Literal>();
            var copyRNegLits = new List<Literal>();

            for (int i = 0; i < 2; i++) 
            {
                trPosLits.Clear();
                trNegLits.Clear();

                if (i == 0) 
                {
                    // See if this clauses positives
                    // unify with the other clauses
                    // negatives
                    trPosLits.AddRange(this.positiveLiterals);
                    trNegLits.AddRange(othC.negativeLiterals);
                } 
                else 
                {
                    // Try the other way round now
                    trPosLits.AddRange(othC.positiveLiterals);
                    trNegLits.AddRange(this.negativeLiterals);
                }

                // Now check to see if they resolve
                IDictionary<Variable, ITerm> copyRBindings = new Dictionary<Variable, ITerm>();
                foreach (var pl in trPosLits) 
                {
                    foreach (var nl in trNegLits) 
                    {
                        copyRBindings.Clear();
                        if (null != _unifier.Unify(pl.AtomicSentence, nl.AtomicSentence, copyRBindings)) 
                        {
                            copyRPosLits.Clear();
                            copyRNegLits.Clear();
                            var found = false;
                            foreach (var l in allPosLits) 
                            {
                                if (!found && pl.Equals(l)) 
                                {
                                    found = true;
                                    continue;
                                }
                                copyRPosLits.Add(_substVisitor.Subst(copyRBindings, l));
                            }
                            found = false;
                            foreach (Literal l in allNegLits) 
                            {
                                if (!found && nl.Equals(l)) 
                                {
                                    found = true;
                                    continue;
                                }
                                copyRNegLits.Add(_substVisitor.Subst(copyRBindings, l));
                            }
                            // Ensure the resolvents are standardized apart
                            var renameSubstitituon = _standardizeApart
                                .GetStandardizeApartResult(copyRPosLits, copyRNegLits, _saIndexical);
                            var c = new Clause(copyRPosLits, copyRNegLits);
                            c.SetProofStep(new ProofStepClauseBinaryResolvent(c, this, othC, copyRBindings, renameSubstitituon));
                            if (this.Immutable) 
                            {
                                c.Immutable = true;
                            }
                            if (!this.IsStandardizedApartCheckRequired()) {
                                c.SetStandardizedApartCheckNotRequired();
                            }
                            resolvents.Add(c);
                        }
                    }
                }
            }

            return resolvents;
        }

        public override string ToString() 
        {
            if (null == stringRep) 
            {
                List<Literal> sortedLiterals = new List<Literal>(literals);
                sortedLiterals.Sort(_literalSorter);

                stringRep = sortedLiterals.ToString();
            }
            return stringRep;
        }

        public override bool Equals(object othObj) 
        {
            if (ReferenceEquals(null, othObj))
            {
                return false;
            }
            if (ReferenceEquals(this, othObj))
            {
                return true;
            }
            if (othObj.GetType() != typeof(Clause))
            {
                return false;
            }
            return Equals((Clause)othObj);
        }

        private void RecalculateIdentity() 
        {
            lock (this) 
            {
                // Sort the literals first based on negation, atomic sentence,
                // constant, function and variable.
                var sortedLiterals = new List<Literal>(literals);
                sortedLiterals.Sort(_literalSorter);

                // All variables are considered the same as regards
                // sorting. Therefore, to determine if two clauses
                // are equivalent you need to determine
                // the # of unique variables they contain and
                // there positions across the clauses
                var ceic = new ClauseEqualityIdentityConstructor(
                        sortedLiterals, _literalSorter);

                this.EqualityIdentity = ceic.GetIdentity();

                // Reset, these as will need to re-calcualte
                // if requested for again, best to only
                // access lazily.
                factors = null;
                nonTrivialFactors = null;
                // Reset the objects string representation
                // until it is requested for.
                stringRep = null;
            }
        }

        private void CalculateFactors(ISet<Clause> parentFactors) 
        {
            nonTrivialFactors = new HashedSet<Clause>();

            IDictionary<Variable, ITerm> theta = new Dictionary<Variable, ITerm>();
            var lits = new List<Literal>();
            for (var i = 0; i < 2; i++) 
            {
                lits.Clear();
                if (i == 0) 
                {
                    // Look at the positive literals
                    lits.AddRange(positiveLiterals);
                } 
                else 
                {
                    // Look at the negative literals
                    lits.AddRange(negativeLiterals);
                }
                for (int x = 0; x < lits.Count; x++) 
                {
                    for (int y = x + 1; y < lits.Count; y++) 
                    {
                        var litX = lits[x];
                        var litY = lits[y];

                        theta.Clear();
                        IDictionary<Variable, ITerm> substitution = _unifier.Unify(
                            litX.AtomicSentence, litY.AtomicSentence, theta);
                        if (substitution != null)
                        {
                            var posLits = new List<Literal>();
                            var negLits = new List<Literal>();
                            if (i == 0) 
                            {
                                posLits.Add(_substVisitor.Subst(substitution, litX));
                            } 
                            else 
                            {
                                negLits.Add(_substVisitor.Subst(substitution, litX));
                            }
                            posLits.AddRange(from pl in this.positiveLiterals
                                             where pl != litX && pl != litY
                                             select _substVisitor.Subst(substitution, pl));
                            negLits.AddRange(from nl in this.negativeLiterals
                                             where nl != litX && nl != litY
                                             select _substVisitor.Subst(substitution, nl));
                            // Ensure the non trivial factor is standardized apart
                            _standardizeApart.GetStandardizeApartResult(posLits, negLits, _saIndexical);
                            Clause c = new Clause(posLits, negLits);
                            c.SetProofStep(new ProofStepClauseFactor(c, this));
                            if (this.Immutable) 
                            {
                                c.Immutable = true;
                            }
                            if (!this.IsStandardizedApartCheckRequired()) 
                            {
                                c.SetStandardizedApartCheckNotRequired();
                            }
                            if (parentFactors == null || !parentFactors.Contains(c)) 
                            {
                                c.CalculateFactors(this.nonTrivialFactors);
                                this.nonTrivialFactors.UnionWith(c.GetFactors());
                            }
                        }
                    }
                }
            }

            factors = new HashedSet<Clause>();
            // Need to add self, even though a non-trivial
            // factor. See: slide 30
            // http://logic.stanford.edu/classes/cs157/2008/lectures/lecture10.pdf
            // for example of incompleteness when
            // trivial factor not included.
            factors.Add(this);
            factors.UnionWith(nonTrivialFactors);
        }

        private Clause SaIfRequired(Clause othClause) 
        {

            // If performing resolution with self
            // then need to standardize apart in
            // order to work correctly.
            if (this.IsStandardizedApartCheckRequired() || this == othClause) 
            {
                var mVariables = _variableCollector.CollectAllVariables(this);
                var oVariables = _variableCollector.CollectAllVariables(othClause);

                var cVariables = new HashSet<Variable>();
                cVariables.UnionWith(mVariables);
                cVariables.UnionWith(oVariables);

                if (cVariables.Count < (mVariables.Count + oVariables.Count)) {
                    othClause = _standardizeApart.GetStandardizeApartResult(othClause, _saIndexical);
                }
            }

            return othClause;
        }

        private IDictionary<string, IList<Literal>> CollectLikeLiterals(ISet<Literal> literals) 
        {
            var likeLiterals = new Dictionary<string, IList<Literal>>();
            foreach (var l in literals) 
            {
                // Want to ensure P(a, b) is considered different than P(a, b, c)
                // i.e. consider an atom's arity P/#.
                var literalName = (l.IsNegativeLiteral() ? "~" : "")
                        + l.AtomicSentence.GetSymbolicName() + "/"
                        + l.AtomicSentence.GetArgs().Count;
                var like = likeLiterals[literalName];
                if (like == null) 
                {
                    like = new List<Literal>();
                    likeLiterals[literalName] = like;
                }
                like.Add(l);
            }
            return likeLiterals;
        }

        private bool CheckSubsumes(Clause othC,IDictionary<string, IList<Literal>> thisToTry, IDictionary<string, IList<Literal>> othCToTry) 
        {
            bool subsumes = false;

            var thisTerms = new List<ITerm>();
            var othCTerms = new List<ITerm>();

            // Want to track possible number of permuations
            var radixs = new List<int>();
            foreach (string literalName in thisToTry.Keys) 
            {
                int sizeT = thisToTry[literalName].Count;
                int sizeO = othCToTry[literalName].Count;

                if (sizeO > 1) {
                    // The following is being used to
                    // track the number of permutations
                    // that can be mapped from the
                    // other clauses like literals to this
                    // clauses like literals.
                    // i.e. n!/(n-r)!
                    // where n=sizeO and r =sizeT
                    for (int i = 0; i < sizeT; i++) {
                        int r = sizeO - i;
                        if (r > 1) {
                            radixs.Add(r);
                        }
                    }
                }
                // Track the terms for this clause
                foreach (var tl in thisToTry[literalName]) 
                {
                    thisTerms.AddRange(tl.AtomicSentence.GetArgs().Cast<ITerm>());
                }
            }

            MixedRadixNumber permutation = null;
            long numPermutations = 1L;
            if (radixs.Count > 0) {
                permutation = new MixedRadixNumber(0, radixs);
                numPermutations = permutation.GetMaxAllowedValue() + 1;
            }
            // Want to ensure none of the othCVariables are
            // part of the key set of a unification as
            // this indicates it is not a legal subsumption.
            var othCVariables = _variableCollector.CollectAllVariables(othC);
            var theta = new Dictionary<Variable, ITerm>();
            var literalPermuations = new List<Literal>();
            for (var l = 0L; l < numPermutations; l++) 
            {
                // Track the other clause's terms for this
                // permutation.
                othCTerms.Clear();
                var radixIdx = 0;
                foreach (string literalName in thisToTry.Keys) 
                {
                    var sizeT = thisToTry[literalName].Count;
                    literalPermuations.Clear();
                    literalPermuations.AddRange(othCToTry[literalName]);
                    var sizeO = literalPermuations.Count;

                    if (sizeO > 1) 
                    {
                        for (var i = 0; i < sizeT; i++) 
                        {
                            var r = sizeO - i;
                            if (r > 1) 
                            {
                                // If not a 1 to 1 mapping then you need
                                // to use the correct permuation
                                int numPos = permutation.GetCurrentNumeralValue(radixIdx);
                                othCTerms.AddRange(literalPermuations[numPos].AtomicSentence.GetArgs().Cast<ITerm>());
                                literalPermuations.RemoveAt(numPos);
                                radixIdx++;
                            } 
                            else 
                            {
                                // is the last mapping, therefore
                                // won't be on the radix
                                othCTerms.AddRange(literalPermuations[0].AtomicSentence.GetArgs().Cast<ITerm>());
                            }
                        }
                    } 
                    else 
                    {
                        // a 1 to 1 mapping
                        othCTerms.AddRange(literalPermuations[0].AtomicSentence.GetArgs().Cast<ITerm>());
                    }
                }

                // Note: on unifier
                // unifier.unify(P(w, x), P(y, z)))={w=y, x=z}
                // unifier.unify(P(y, z), P(w, x)))={y=w, z=x}
                // Therefore want this clause to be the first
                // so can do the othCVariables check for an invalid
                // subsumes.
                theta.Clear();
                if (null != _unifier.Unify((IFOLNode) thisTerms, (IFOLNode) othCTerms, theta)) 
                {
                    var containsAny = theta.Keys.Any(othCVariables.Contains);
                    if (!containsAny) 
                    {
                        subsumes = true;
                        break;
                    }
                }

                // If there is more than 1 mapping
                // keep track of where I am in the
                // possible number of mapping permutations.
                if (null != permutation) 
                {
                    permutation.Increment();
                }
            }

            return subsumes;
        }

        public bool Equals(Clause other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.EqualityIdentity, this.EqualityIdentity);
        }

        public override int GetHashCode()
        {
            return (this.EqualityIdentity != null ? this.EqualityIdentity.GetHashCode() : 0);
        }
    }

    class LiteralsSorter : Comparer<Literal> 
    {
        public override int Compare(Literal o1, Literal o2) 
        {
            var rVal = 0;
            // If literals are not negated the same
            // then positive literals are considered
            // (by convention here) to be of higher
            // order than negative literals
            if (o1.IsPositiveLiteral() != o2.IsPositiveLiteral()) 
            {
                if (o1.IsPositiveLiteral()) 
                {
                    return 1;
                }
                return -1;
            }

            // Check their symbolic names for order first
            rVal = o1.AtomicSentence.GetSymbolicName().CompareTo(o2.AtomicSentence.GetSymbolicName());

            // If have same symbolic names
            // then need to compare individual arguments
            // for order.
            if (0 == rVal) 
            {
                rVal = this.CompareArgs(o1.AtomicSentence.GetArgs(), o2.AtomicSentence.GetArgs());
            }

            return rVal;
        }

        private int CompareArgs(IList<IFOLNode> args1, IList<IFOLNode> args2) 
        {
            // Compare argument sizes first
            var rVal = args1.Count - args2.Count;

            if (rVal == 0 && args1.Count > 0) 
            {
                // Move forward and compare the
                // first arguments
                ITerm t1 = (ITerm) args1[0];
                ITerm t2 = (ITerm) args2[0];

                if (t1.GetType() == t2.GetType()) 
                {
                    // Note: Variables are considered to have
                    // the same order
                    if (t1 is Constant) 
                    {
                        rVal = t1.GetSymbolicName().CompareTo(t2.GetSymbolicName());
                    } 
                    else if (t1 is Function) 
                    {
                        rVal = t1.GetSymbolicName().CompareTo(t2.GetSymbolicName());
                        if (0 == rVal) 
                        {
                            // Same function names, therefore
                            // compare the function arguments
                            rVal = this.CompareArgs(t1.GetArgs(), t2.GetArgs());
                        }
                    }

                    // If the first args are the same
                    // then compare the ordering of the
                    // remaining arguments
                    if (0 == rVal) 
                    {
                        rVal = this.CompareArgs(args1.Skip(1).ToList(), args2.Skip(1).ToList());
                    }
                } 
                else 
                {
                    // Order for different Terms is:
                    // Constant > Function > Variable
                    if (t1 is Constant) 
                    {
                        rVal = 1;
                    } 
                    else if (t2 is Constant) 
                    {
                        rVal = -1;
                    } 
                    else if (t1 is Function) 
                    {
                        rVal = 1;
                    } 
                    else 
                    {
                        rVal = -1;
                    }
                }
            }

            return rVal;
        }
    }

    class ClauseEqualityIdentityConstructor : IFOLVisitor 
    {
        private StringBuilder identity = new StringBuilder();
        private int noVarPositions;
        private int[] clauseVarCounts;
        private int currentLiteral;
        private IDictionary<string, IList<int>> varPositions = new Dictionary<string, IList<int>>();

        public ClauseEqualityIdentityConstructor(IList<Literal> literals, LiteralsSorter sorter) 
        {
            clauseVarCounts = new int[literals.Count];

            foreach (Literal l in literals) 
            {
                if (l.IsNegativeLiteral()) 
                {
                    identity.Append("~");
                }
                identity.Append(l.AtomicSentence.GetSymbolicName());
                identity.Append("(");
                var firstTerm = true;
                foreach (var t in l.AtomicSentence.GetArgs()) 
                {
                    if (firstTerm) 
                    {
                        firstTerm = false;
                    } 
                    else 
                    {
                        identity.Append(",");
                    }
                    t.Accept(this, null);
                }
                identity.Append(")");
                currentLiteral++;
            }

            int min, max;
            min = max = 0;
            for (var i = 0; i < literals.Count; i++) 
            {
                var incITo = i;
                var next = i + 1;
                max += clauseVarCounts[i];
                while (next < literals.Count) 
                {
                    if (0 != sorter.Compare(literals[i], literals[next])) 
                    {
                        break;
                    }
                    max += clauseVarCounts[next];
                    incITo = next; // Need to skip to the end of the range
                    next++;
                }
                // This indicates two or more literals are identical
                // except for variable naming (note: identical
                // same name would be removed as are working
                // with sets so don't need to worry about this).
                if ((next - i) > 1) 
                {
                    // Need to check each variable
                    // and if it has a position within the
                    // current min/max range then need
                    // to include its alternative
                    // sort order positions as well
                    foreach (var key in varPositions.Keys) 
                    {
                        var positions = varPositions[key];
                        var additPositions = new List<int>();
                        // Add then subtract for all possible
                        // positions in range
                        foreach (int pos in positions) 
                        {
                            if (pos < min || pos >= max)
                            {
                                continue;
                            }
                            var pPos = pos;
                            var nPos = pos;
                            for (var candSlot = i; candSlot < (next - 1); candSlot++) 
                            {
                                pPos += this.clauseVarCounts[i];
                                if (pPos >= min && pPos < max) 
                                {
                                    if (!positions.Contains(pPos) && !additPositions.Contains(pPos)) 
                                    {
                                        additPositions.Add(pPos);
                                    }
                                }
                                nPos -= this.clauseVarCounts[i];
                                if (nPos >= min && nPos < max) 
                                {
                                    if (!positions.Contains(nPos) && !additPositions.Contains(nPos)) 
                                    {
                                        additPositions.Add(nPos);
                                    }
                                }
                            }
                        }
                        foreach (var p in additPositions)
                        {
                            positions.Add(p);
                        }
                    }
                }
                min = max;
                i = incITo;
            }

            // Determine the maxWidth
            int maxWidth = 1;
            while (noVarPositions >= 10) 
            {
                noVarPositions = noVarPositions / 10;
                maxWidth++;
            }
            var format = "%0" + maxWidth + "d";

            // Sort the individual position lists
            // And then add their string representations
            // together
            var varOffsets = new List<string>();
            foreach (string key in varPositions.Keys) 
            {
                var positions = varPositions[key].ToList();
                positions.Sort();
                varOffsets.Add(positions.Aggregate(
                    new StringBuilder(), 
                    (sb, pos) => sb.Append(String.Format(format, pos)), 
                    sb => sb.ToString()
                    ));
            }
            varOffsets.Sort();
            for (int i = 0; i < varOffsets.Count; i++) 
            {
                identity.Append(varOffsets[i]);
                if (i < (varOffsets.Count - 1)) 
                {
                    identity.Append(",");
                }
            }
        }

        public string GetIdentity() 
        {
            return identity.ToString();
        }

        public object VisitVariable(Variable var, object arg) 
        {
            // All variables will be marked with an *
            identity.Append("*");

            IList<int> positions;
            if (varPositions.ContainsKey(var.Value))
            {
                positions = this.varPositions[var.Value];
            }
            else 
            {
                positions = new List<int>();
                varPositions[var.Value] = positions;
            }
            positions.Add(noVarPositions);

            noVarPositions++;
            clauseVarCounts[currentLiteral]++;
            return var;
        }

        public object VisitConstant(Constant constant, object arg) 
        {
            identity.Append(constant.Value);
            return constant;
        }

        public object VisitFunction(Function function, object arg) 
        {
            var firstTerm = true;
            identity.Append(function.GetFunctionName());
            identity.Append("(");
            foreach (ITerm t in function.Terms) 
            {
                if (firstTerm) 
                {
                    firstTerm = false;
                } 
                else 
                {
                    this.identity.Append(",");
                }
                t.Accept(this, arg);
            }
            identity.Append(")");

            return function;
        }

        public object VisitPredicate(Predicate predicate, object arg) 
        {
            throw new InvalidOperationException("Should not be called");
        }

        public object VisitTermEquality(TermEquality equality, object arg) 
        {
            throw new InvalidOperationException("Should not be called");
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {
                    throw new InvalidOperationException("Should not be called");
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
            throw new InvalidOperationException("Should not be called");
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            throw new InvalidOperationException("Should not be called");
        }
    }
}
