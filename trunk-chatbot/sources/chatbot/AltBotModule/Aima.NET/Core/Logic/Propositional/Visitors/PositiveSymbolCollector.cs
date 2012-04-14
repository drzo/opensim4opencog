using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class PositiveSymbolCollector : BasicTraverser 
    {
        public override object VisitSymbol(Symbol symbol, ISet<Sentence> arg)
        {
            arg.Add(symbol);// add ALL symbols not discarded by the visitNotSentence
            // mathod
            return arg;
        }

        public override object VisitNotSentence(UnarySentence ns, ISet<Sentence> arg) 
        {
            if (ns.Negated is Symbol) 
            {
                // do nothing .do NOT add a negated Symbol
            } else
            {
                //arg.UnionWith((ISet<Symbol>)ns.Negated.Accept(this, arg));
                arg.UnionWith((ISet<Sentence>)ns.Negated.Accept(this, arg));
            }
            return arg;
        }

        public ISet<Symbol> GetPositiveSymbolsIn(Sentence sentence) {
            return (ISet<Symbol>)sentence.Accept(this, new HashedSet<Sentence>());
        }
    }
}
