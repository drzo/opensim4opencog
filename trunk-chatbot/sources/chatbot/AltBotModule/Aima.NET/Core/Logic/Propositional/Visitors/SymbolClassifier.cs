using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class SymbolClassifier
    {

        //TODO: make sure that these formerly Set operations changed to List operations are ok
        public IList<Symbol> GetPositiveSymbolsIn(Sentence sentence)
        {
            return new PositiveSymbolCollector().GetPositiveSymbolsIn(sentence).ToList();
        }

        public IList<Symbol> GetNegativeSymbolsIn(Sentence sentence)
        {
            return new NegativeSymbolCollector().GetNegativeSymbolsIn(sentence).ToList();
        }

        public IList<Symbol> GetPureNegativeSymbolsIn(Sentence sentence)
        {
            IList<Symbol> allNegatives = this.GetNegativeSymbolsIn(sentence);
            IList<Symbol> allPositives = this.GetPositiveSymbolsIn(sentence);
            return new List<Symbol>(allNegatives.Except(allPositives));
        }

        public IList<Symbol> GetPurePositiveSymbolsIn(Sentence sentence)
        {
            IList<Symbol> allNegatives = this.GetNegativeSymbolsIn(sentence);
            IList<Symbol> allPositives = this.GetPositiveSymbolsIn(sentence);
            return new List<Symbol>(allPositives.Except(allNegatives));
        }

        public IList<Symbol> GetPureSymbolsIn(Sentence sentence)
        {
            IList<Symbol> allPureNegatives = this.GetPureNegativeSymbolsIn(sentence);
            IList<Symbol> allPurePositives = this.GetPurePositiveSymbolsIn(sentence);
            return new List<Symbol>(allPurePositives.Union(allPureNegatives));
        }

        public IList<Symbol> GetImpureSymbolsIn(Sentence sentence)
        {
            IList<Symbol> allNegatives = this.GetNegativeSymbolsIn(sentence);
            IList<Symbol> allPositives = this.GetPositiveSymbolsIn(sentence);
            return new List<Symbol>(allPositives.Intersect(allNegatives));
        }

        public IList<Symbol> GetSymbolsIn(Sentence sentence)
        {
            return new SymbolCollector().GetSymbolsIn(sentence).ToList();
        }
    }
}
