using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System.Collections;

    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    using Aima.Core.Util;

    public class PLFCEntails {

        private Dictionary<HornClause, int> count;

        private Dictionary<Symbol, bool?> inferred;

        private Stack<Symbol> agenda;

        public PLFCEntails() {
            count = new Dictionary<HornClause, int>();
            this.inferred = new Dictionary<Symbol, bool?>();
            agenda = new Stack<Symbol>();
        }

        //TODO: update with better name
        public bool PlfcEntails(KnowledgeBase kb, string s) {
            return this.PlfcEntails(kb, new Symbol(s));
        }

        public bool PlfcEntails(KnowledgeBase kb, Symbol q) {
            IList<HornClause> hornClauses = this.AsHornClauses(kb.Sentences);
            while (agenda.Count != 0) {
                Symbol p = agenda.Pop();
                while (!this.GetInferred(p))
                {
                    this.inferred[p] =true;

                    foreach (HornClause hornClause in
                        hornClauses.Where(hornClause => hornClause.PremisesContainsSymbol(p)))
                    {
                        this.DecrementCount(hornClause);
                        if (!this.CountIsZero(hornClause))
                        {
                            continue;
                        }
                        if (hornClause.Head.Equals(q)) {
                            return true;
                        }
                        this.agenda.Push(hornClause.Head);
                    }
                }
            }
            return false;
        }

        private IList<HornClause> AsHornClauses(IList<Sentence> sentences) {
            return sentences.Select(sentence => new HornClause(sentence, agenda, count, inferred)).ToList();
        }

        private bool CountIsZero(HornClause hornClause) {

            return (count[hornClause]) == 0;
        }

        private void DecrementCount(HornClause hornClause) {
            int value = (count[hornClause]);
            count[hornClause] = value - 1;

        }

        private bool GetInferred(Symbol p) {
            bool? value = this.inferred[p];
            return ((value == null) || value == true);
        }

        public class HornClause {
            IList<Symbol> premiseSymbols;

            public Symbol Head { get; private set; }

            public HornClause(Sentence sentence, Stack<Symbol> agenda, 
                Dictionary<HornClause, int> count, Dictionary<Symbol, bool?> inferred) {
                if (sentence is Symbol) 
                {
                    this.Head = (Symbol) sentence;
                    agenda.Push(this.Head);
                    premiseSymbols = new List<Symbol>();
                    count[this] = 0;
                    inferred[this.Head] = false;
                } else if (!this.IsImpliedSentence(sentence)) {
                    throw new ApplicationException("Sentence " + sentence + " is not a horn clause");

                } else {
                    BinarySentence bs = (BinarySentence) sentence;
                    this.Head = (Symbol) bs.Second;
                    inferred[this.Head] = false;
                    ISet<Symbol> symbolsInPremise = new SymbolCollector().GetSymbolsIn(bs.First);
                    foreach(var symbol in symbolsInPremise)
                    {
                        inferred[symbol] = false;
                    }
                    premiseSymbols = symbolsInPremise.ToList();

                    count[this] = premiseSymbols.Count;
                }

            }

            private bool IsImpliedSentence(Sentence sentence) {
                return ((sentence is BinarySentence) && ((BinarySentence) sentence)
                        .Operator.Equals("=>"));
            }

            public bool PremisesContainsSymbol(Symbol q) {
                return premiseSymbols.Contains(q);
            }

            public IList<Symbol> GetPremiseSymbols() {
                return premiseSymbols;
            }

            public override bool Equals(object o) {
                if (ReferenceEquals(null, o))
                {
                    return false;
                }
                if (ReferenceEquals(this, o))
                {
                    return true;
                }
                if (o.GetType() != typeof(HornClause))
                {
                    return false;
                }
                return Equals((HornClause)o);
            }

            public override string ToString() {
                return this.premiseSymbols + " => " + this.Head;
            }

            public bool Equals(HornClause other)
            {
                if (ReferenceEquals(null, other))
                {
                    return false;
                }
                if (ReferenceEquals(this, other))
                {
                    return true;
                }
                return Equals(other.premiseSymbols, this.premiseSymbols) && Equals(other.Head, this.Head);
            }

            public override int GetHashCode()
            {
                unchecked
                {
                    return ((this.premiseSymbols != null ? this.premiseSymbols.GetHashCode() : 0) * 397) ^ (this.Head != null ? this.Head.GetHashCode() : 0);
                }
            }
        }
    }
}
