using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;
namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System.Collections;

    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    using Aima.Core.Util;

    public class TTEntails
    {
        //TODO: Update this method with better name
        public bool TtEntails(KnowledgeBase kb, String alpha)
        {
            Sentence kbSentence = kb.AsSentence();
            Sentence querySentence = (Sentence)new PEParser().Parse(alpha);
            SymbolCollector collector = new SymbolCollector();
            ISet<Symbol> kbSymbols = collector.GetSymbolsIn(kbSentence);
            ISet<Symbol> querySymbols = collector.GetSymbolsIn(querySentence);
            IList<Symbol> symbolList = kbSymbols.Union(querySymbols).ToList();
            return ttCheckAll(kbSentence, querySentence, symbolList, new Model());
        }

        public bool ttCheckAll(Sentence kbSentence, Sentence querySentence,
                IList<Symbol> symbols, Model model)
        {
            if (symbols.Count == 0)
            {
                if (model.IsTrue(kbSentence))
                {
                    // System.out.println("#");
                    return model.IsTrue(querySentence);
                }
                else
                {
                    // System.out.println("0");
                    return true;
                }
            }
            else
            {
                Symbol symbol = Util.First(symbols);
                IList<Symbol> rest = Util.Rest(symbols);

                Model trueModel = model.Extend(new Symbol(symbol.Value), true);
                Model falseModel = model.Extend(new Symbol(symbol.Value), false);
                return (ttCheckAll(kbSentence, querySentence, rest, trueModel) && (ttCheckAll(
                        kbSentence, querySentence, rest, falseModel)));
            }
        }
    }
}
