namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using Iesi.Collections.Generic;

    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    using Aima.Core.Util;
    using System.Linq;

    public class DPLL 
    {

        private static readonly Converter<Symbol> SymbolConverter = new Converter<Symbol>();

        public bool DPLLSatisfiable(Sentence s) {

            return this.DPLLSatisfiable(s, new Model());
        }

        public bool DPLLSatisfiable(string str) {
            Sentence sen = (Sentence) new PEParser().Parse(str);
            return this.DPLLSatisfiable(sen, new Model());
        }

        public bool DPLLSatisfiable(Sentence s, Model m) {
            ISet<Sentence> clauses = new CNFClauseGatherer().GetClausesFrom(new CNFTransformer().Transform(s));
            IList<Symbol> symbols = SymbolConverter.SetToList(new SymbolCollector().GetSymbolsIn(s));
            
            // System.out.println(" numberOfSymbols = " + symbols.Count);
            return Dpll(clauses, symbols, m);
        }

        public IList<Sentence> ClausesWithNonTrueValues(IList<Sentence> clauseList, Model model) {
            IList<Sentence> clausesWithNonTrueValues = new List<Sentence>();
            for (int i = 0; i < clauseList.Count; i++) {
                Sentence clause = clauseList[i];
                if (!(this.IsClauseTrueInModel(clause, model))) {
                    if (!(clausesWithNonTrueValues.Contains(clause))) {// defensive
                        // programming not really necessary
                        clausesWithNonTrueValues.Add(clause);
                    }
                }

            }
            return clausesWithNonTrueValues;
        }

        public SymbolValuePair FindPureSymbolValuePair(IList<Sentence> clauseList,
                Model model) {
            IList<Sentence> clausesWithNonTrueValues = ClausesWithNonTrueValues(clauseList, model);
            Sentence nonTrueClauses = LogicUtils.ChainWith("AND", clausesWithNonTrueValues);
            
            // System.out.println("Unsatisfied clauses = "
            // + clausesWithNonTrueValues.Count);
            ISet<Symbol> symbolsAlreadyAssigned = model.GetAssignedSymbols();

            // debug
            // IList symList = asList(symbolsAlreadyAssigned);
            //
            // System.out.println(" assignedSymbols = " + symList.Count);
            // if (symList.Count == 52) {
            // System.out.println("untrue clauses = " + clausesWithNonTrueValues);
            // System.out.println("model= " + model);
            // }

            // debug
            IList<Symbol> purePositiveSymbols = new SymbolClassifier().GetPurePositiveSymbolsIn(nonTrueClauses)
                .Except(symbolsAlreadyAssigned).ToList();

            IList<Symbol> pureNegativeSymbols =
                new SymbolClassifier().GetPureNegativeSymbolsIn(nonTrueClauses).Except(symbolsAlreadyAssigned).ToList();
            // if none found return "not found
            if ((purePositiveSymbols.Count == 0)
                    && (pureNegativeSymbols.Count == 0)) {
                return new SymbolValuePair();// automatically set to null values
            } else {
                if (purePositiveSymbols.Count > 0) {
                    Symbol symbol = new Symbol((purePositiveSymbols[0])
                            .Value);
                    if (pureNegativeSymbols.Contains(symbol)) {
                        throw new ApplicationException("Symbol " + symbol.Value
                                + "misclassified");
                    }
                    return new SymbolValuePair(symbol, true);
                } else {
                    Symbol symbol = new Symbol((pureNegativeSymbols[0])
                            .Value);
                    if (purePositiveSymbols.Contains(symbol)) {
                        throw new ApplicationException("Symbol " + symbol.Value
                                + "misclassified");
                    }
                    return new SymbolValuePair(symbol, false);
                }
            }
        }

        private bool Dpll(ISet<Sentence> clauses, IList<Symbol> symbols, Model model) {
            // IList<Sentence> clauseList = asList(clauses);
            IList<Sentence> clauseList = clauses.ToList();
            // System.out.println("clauses are " + clauses.toString());
            // if all clauses are true return true;
            if (this.AreAllClausesTrue(model, clauseList)) {
                // System.out.println(model.toString());
                return true;
            }
            // if even one clause is false return false
            if (this.IsEvenOneClauseFalse(model, clauseList)) {
                // System.out.println(model.toString());
                return false;
            }
            // System.out.println("At least one clause is unknown");
            // try to find a unit clause
            SymbolValuePair svp = this.FindPureSymbolValuePair(clauseList, model);
            IList<Symbol> newSymbols;
            if (svp.NotNull()) {
                newSymbols = symbols.ToList();
                newSymbols.Remove(new Symbol(svp.Symbol.Value));
                Model newModel = model.Extend(new Symbol(svp.Symbol.Value),
                        svp.Value);
                return Dpll(clauses, newSymbols, newModel);
            }

            SymbolValuePair svp2 = this.FindUnitClause(clauseList, model);
            if (svp2.NotNull()) {
                newSymbols = symbols.ToList();
                newSymbols.Remove(new Symbol(svp2.Symbol.Value));
                Model newModel = model.Extend(new Symbol(svp2.Symbol.Value),
                        svp2.Value);
                return Dpll(clauses, newSymbols, newModel);
            }

            Symbol symbol = symbols[0];
            // System.out.println("default behaviour selecting " + symbol);
            newSymbols = symbols.ToList();
            newSymbols.RemoveAt(0);
            return (Dpll(clauses, newSymbols, model.Extend(symbol, true)) || Dpll(
                    clauses, newSymbols, model.Extend(symbol, false)));
        }

        private bool IsEvenOneClauseFalse(Model model, IList<Sentence> clauseList)
        {
            return clauseList.Any(model.IsFalse);
        }

        private bool AreAllClausesTrue(Model model, IList<Sentence> clauseList)
        {
            return clauseList.All(clause => this.IsClauseTrueInModel(clause, model));
        }

        private bool IsClauseTrueInModel(Sentence clause, Model model) {
            IList<Symbol> positiveSymbols = new SymbolClassifier().GetPositiveSymbolsIn(clause).ToList();
            IList<Symbol> negativeSymbols = new SymbolClassifier().GetNegativeSymbolsIn(clause).ToList();

            if (positiveSymbols.Any(symbol => (model.IsTrue(symbol))))
            {
                return true;
            }
            return negativeSymbols.Any(symbol => (model.IsFalse(symbol)));
        }

        private SymbolValuePair FindUnitClause(IList<Sentence> clauseList, Model model)
        {
            foreach (Sentence t in clauseList)
            {
                var clause = t;
                if ((clause is Symbol)
                    && (!(model.GetAssignedSymbols().Contains((Symbol)clause)))) {
                        // System.out.println("found unit clause - assigning");
                        return new SymbolValuePair(new Symbol(((Symbol) clause)
                            .Value), true);
                    }

                if (!(clause is UnarySentence))
                {
                    continue;
                }
                var sentence = (UnarySentence) clause;
                var negated = sentence.Negated;
                if ((negated is Symbol)
                    && (!(model.GetAssignedSymbols().Contains((Symbol)negated)))) {
                        // System.out.println("found unit clause type 2 -
                        // assigning");
                        return new SymbolValuePair(new Symbol(((Symbol) negated)
                            .Value), false);
                    }
            }

            return new SymbolValuePair();// failed to find any unit clause;
        }

        public class SymbolValuePair {
            public Symbol Symbol { get; set; }

            public bool? Value { get; set; }

            public SymbolValuePair() {
                // represents "No Symbol found with a bool value that makes all
                // its literals true
                this.Symbol = null;
                this.Value = null;
            }

            public SymbolValuePair(Symbol symbol, bool value) {
                // represents "Symbol found with a bool value that makes all
                // its literals true
                this.Symbol = symbol;
                this.Value = value;
            }

            public bool NotNull() {
                return (this.Symbol != null) && (this.Value != null);
            }

            public override string ToString() {
                string symbolString, valueString;
                if (this.Symbol == null) {
                    symbolString = "NULL";
                } else {
                    symbolString = this.Symbol.ToString();
                }
                if (this.Value == null) {
                    valueString = "NULL";
                } else {
                    valueString = this.Value.ToString();
                }
                return symbolString + " -> " + valueString;
            }
        }
    }
}
