using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.Inference.Proof;
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class StandardizeApart
    {
        private VariableCollector variableCollector = null;
        private SubstVisitor substVisitor = null;

        public StandardizeApart()
        {
            variableCollector = new VariableCollector();
            substVisitor = new SubstVisitor();
        }

        public StandardizeApart(VariableCollector variableCollector,
                SubstVisitor substVisitor)
        {
            this.variableCollector = variableCollector;
            this.substVisitor = substVisitor;
        }

        // Note: see page 327.
        public StandardizeApartResult GetStandardizeApartResult(ISentence aSentence,
                IStandardizeApartIndexical standardizeApartIndexical) 
        {
            var toRename = variableCollector.CollectAllVariables(aSentence);
            IDictionary<Variable, ITerm> renameSubstitution = new Dictionary<Variable, ITerm>();
            IDictionary<Variable, ITerm> reverseSubstitution = new Dictionary<Variable, ITerm>();

            foreach (var var in toRename) 
            {
                Variable v = null;
                do {
                    v = new Variable(standardizeApartIndexical.GetPrefix()
                            + standardizeApartIndexical.GetNextIndex());
                    // Ensure the new variable name is not already
                    // accidentally used in the sentence
                } while (toRename.Contains(v));

                renameSubstitution[var] = v;
                reverseSubstitution[v] = var;
            }

            var standardized = substVisitor.Subst(renameSubstitution, aSentence);

            return new StandardizeApartResult(aSentence, standardized,
                    renameSubstitution, reverseSubstitution);
        }

        public Clause GetStandardizeApartResult(Clause clause, IStandardizeApartIndexical standardizeApartIndexical) 
        {

            var toRename = variableCollector.CollectAllVariables(clause);
            IDictionary<Variable, ITerm> renameSubstitution = new Dictionary<Variable, ITerm>();

            foreach (var var in toRename) 
            {
                Variable v = null;
                do 
                {
                    v = new Variable(standardizeApartIndexical.GetPrefix()
                            + standardizeApartIndexical.GetNextIndex());
                    // Ensure the new variable name is not already
                    // accidentally used in the sentence
                } while (toRename.Contains(v));

                renameSubstitution[var] = v;
            }

            if (renameSubstitution.Count > 0) 
            {
                var literals = clause.GetLiterals().Select(l => this.substVisitor.Subst(renameSubstitution, l)).ToList();

                var renamed = new Clause(literals);
                renamed.SetProofStep(new ProofStepRenaming(renamed, clause.GetProofStep()));
                return renamed;
            }

            return clause;
        }

        public Chain GetStandardizeApartResult(Chain chain, IStandardizeApartIndexical standardizeApartIndexical) 
        {

            var toRename = variableCollector.CollectAllVariables(chain);
            IDictionary<Variable, ITerm> renameSubstitution = new Dictionary<Variable, ITerm>();

            foreach (var var in toRename) 
            {
                Variable v = null;
                do 
                {
                    v = new Variable(standardizeApartIndexical.GetPrefix()
                            + standardizeApartIndexical.GetNextIndex());
                    // Ensure the new variable name is not already
                    // accidentally used in the sentence
                } while (toRename.Contains(v));

                renameSubstitution[var] = v;
            }

            if (renameSubstitution.Count > 0) 
            {
                var lits = (from l in chain.GetLiterals()
                            let atom = (IAtomicSentence)this.substVisitor.Subst(renameSubstitution, l.AtomicSentence)
                            select l.NewInstance(atom)).ToList();

                var renamed = new Chain(lits);

                renamed.SetProofStep(new ProofStepRenaming(renamed, chain.ProofStep));

                return renamed;
            }

            return chain;
        }

        public IDictionary<Variable, ITerm> GetStandardizeApartResult(IList<Literal> l1Literals,
                IList<Literal> l2Literals, IStandardizeApartIndexical standardizeApartIndexical) 
        {
            var toRename = new HashSet<Variable>();

            foreach (var pl in l1Literals) 
            {
                toRename.UnionWith(variableCollector.CollectAllVariables(pl.AtomicSentence));
            }
            foreach (var nl in l2Literals) 
            {
                toRename.UnionWith(variableCollector.CollectAllVariables(nl.AtomicSentence));
            }

            var renameSubstitution = new Dictionary<Variable, ITerm>();

            foreach (var var in toRename) 
            {
                Variable v = null;
                do 
                {
                    v = new Variable(standardizeApartIndexical.GetPrefix()
                            + standardizeApartIndexical.GetNextIndex());
                    // Ensure the new variable name is not already
                    // accidentally used in the sentence
                } 
                while (toRename.Contains(v));

                renameSubstitution[var] = v;
            }

            var posLits = new List<Literal>();
            var negLits = new List<Literal>();

            foreach (var pl in l1Literals) 
            {
                posLits.Add(substVisitor.Subst(renameSubstitution, pl));
            }
            foreach (Literal nl in l2Literals) 
            {
                negLits.Add(substVisitor.Subst(renameSubstitution, nl));
            }

            l1Literals.Clear();
            foreach (var l in posLits)
            {
                l1Literals.Add(l);
            }

            l2Literals.Clear();
            foreach (var l in negLits)
            {
                l2Literals.Add(l);
            }

            return renameSubstitution;
        }
    }

}
