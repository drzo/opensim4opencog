using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Propositional.Algorithms
{
    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    using Aima.Core.Util;

    public class PLResolution
    {

        public bool PlResolution(KnowledgeBase kb, string alpha)
        {
            return this.PlResolution(kb, (Sentence)new PEParser().Parse(alpha));
        }

        public bool PlResolution(KnowledgeBase kb, Sentence alpha)
        {
            Sentence kBAndNotAlpha = new BinarySentence("AND", kb.AsSentence(),
                    new UnarySentence(alpha));
            IList<Sentence> clauses = new CNFClauseGatherer()
                    .GetClausesFrom(new CNFTransformer().Transform(kBAndNotAlpha)).ToList();
            clauses = this.FilterOutClausesWithTwoComplementaryLiterals(clauses);
            var newClauses = new List<Sentence>();
            while (true)
            {
                IList<IList<Sentence>> pairs = this.GetCombinationPairs(clauses.ToList());

                for (int i = 0; i < pairs.Count; i++)
                {
                    IList<Sentence> pair = pairs[i];
                    // System.out.println("pair number" + i+" of "+pairs.Count);
                    IList<Sentence> resolvents = this.PLResolve(pair[0], pair[1]);
                    resolvents = this.FilterOutClausesWithTwoComplementaryLiterals(resolvents);

                    if (resolvents.Contains(new Symbol("EMPTY_CLAUSE")))
                    {
                        return true;
                    }
                    newClauses = newClauses.Union(resolvents).ToList();
                    // System.out.println("clauseslist size = " +clauses.Count);

                }
                if (newClauses.Intersect(clauses).Count() == newClauses.Count)
                {// subset test
                    return false;
                }
                clauses = newClauses.Union(clauses).ToList();
                clauses = this.FilterOutClausesWithTwoComplementaryLiterals(clauses);
            }

        }

        public IList<Sentence> PLResolve(Sentence clause1, Sentence clause2)
        {
            var cs = new ClauseSymbols(clause1, clause2);

            return cs.GetComplementedSymbols().Select(symbol => this.CreateResolventClause(cs, symbol)).ToList();
        }

        //TODO: create a better name
        public bool PlResolution(string kbs, string alphaString)
        {
            KnowledgeBase kb = new KnowledgeBase();
            kb.Tell(kbs);
            Sentence alpha = (Sentence)new PEParser().Parse(alphaString);
            return this.PlResolution(kb, alpha);
        }

        private IList<Sentence> FilterOutClausesWithTwoComplementaryLiterals(IList<Sentence> clauses)
        {
            var classifier = new SymbolClassifier();
            return (from clause in clauses
                    let positiveSymbols = classifier.GetPositiveSymbolsIn(clause)
                    let negativeSymbols = classifier.GetNegativeSymbolsIn(clause)
                    where positiveSymbols.Intersect(negativeSymbols).Count() == 0
                    select clause).ToList();
        }

        private Sentence CreateResolventClause(ClauseSymbols cs, Symbol toRemove)
        {
            var positiveSymbols = cs.Clause1PositiveSymbols.Union(cs.Clause2PositiveSymbols).ToList();
            var negativeSymbols = cs.Clause1NegativeSymbols.Union(cs.Clause2NegativeSymbols).ToList();
            if (positiveSymbols.Contains(toRemove))
            {
                positiveSymbols.Remove(toRemove);
            }
            if (negativeSymbols.Contains(toRemove))
            {
                negativeSymbols.Remove(toRemove);
            }

            positiveSymbols.Sort(this.CompareSymbols);
            negativeSymbols.Sort(this.CompareSymbols);

            IList<Sentence> sentences = new List<Sentence>();
            for (int i = 0; i < positiveSymbols.Count; i++)
            {
                sentences.Add(positiveSymbols[i]);
            }
            for (int i = 0; i < negativeSymbols.Count; i++)
            {
                sentences.Add(new UnarySentence(negativeSymbols[i]));
            }
            if (sentences.Count == 0)
            {
                return new Symbol("EMPTY_CLAUSE"); // == empty clause
            }
            else
            {
                return LogicUtils.ChainWith("OR", sentences);
            }

        }

        public int CompareSymbols(Symbol symbol1, Symbol symbol2) 
        {
            return symbol1.Value.CompareTo(symbol2.Value);
        }

        private IList<IList<Sentence>> GetCombinationPairs(IList<Sentence> clausesList)
        {
            int odd = clausesList.Count % 2;
            int midpoint = 0;
            if (odd == 1)
            {
                midpoint = (clausesList.Count / 2) + 1;
            }
            else
            {
                midpoint = (clausesList.Count / 2);
            }

            IList<IList<Sentence>> pairs = new List<IList<Sentence>>();
            for (int i = 0; i < clausesList.Count; i++)
            {
                for (int j = i; j < clausesList.Count; j++)
                {
                    IList<Sentence> pair = new List<Sentence>();
                    Sentence first = clausesList[i];
                    Sentence second = clausesList[j];

                    if (!(first.Equals(second)))
                    {
                        pair.Add(first);
                        pair.Add(second);
                        pairs.Add(pair);
                    }
                }
            }
            return pairs;
        }

        class ClauseSymbols
        {
            public IList<Symbol> Clause1Symbols { get; private set; }

            public IList<Symbol> Clause1PositiveSymbols { get; private set; }

            public IList<Symbol> Clause1NegativeSymbols { get; private set; }

            public IList<Symbol> Clause2Symbols { get; private set; }

            public IList<Symbol> Clause2PositiveSymbols { get; private set; }

            public IList<Symbol> Clause2NegativeSymbols { get; private set; }

            public IList<Symbol> PositiveInClause1NegativeInClause2 { get; private set; }

            public IList<Symbol> NegativeInClause1PositiveInClause2 { get; private set; }

            public ClauseSymbols(Sentence clause1, Sentence clause2)
            {

                SymbolClassifier classifier = new SymbolClassifier();

                this.Clause1Symbols = classifier.GetSymbolsIn(clause1);
                this.Clause1PositiveSymbols = classifier.GetPositiveSymbolsIn(clause1);
                this.Clause1NegativeSymbols = classifier.GetNegativeSymbolsIn(clause1);

                this.Clause2Symbols = classifier.GetSymbolsIn(clause2);
                this.Clause2PositiveSymbols = classifier.GetPositiveSymbolsIn(clause2);
                this.Clause2NegativeSymbols = classifier.GetNegativeSymbolsIn(clause2);

                this.PositiveInClause1NegativeInClause2 = 
                    this.Clause1PositiveSymbols.Intersect(this.Clause2NegativeSymbols).ToList();
                this.NegativeInClause1PositiveInClause2 =
                    this.Clause1NegativeSymbols.Intersect(this.Clause2PositiveSymbols).ToList();

            }

            public IList<Symbol> GetComplementedSymbols()
            {
                return this.PositiveInClause1NegativeInClause2.Union(this.NegativeInClause1PositiveInClause2).ToList();
            }

        }
    }
}
