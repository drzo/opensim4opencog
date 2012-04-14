using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class NegativeSymbolCollector : BasicTraverser 
    {
        public override object VisitNotSentence(UnarySentence ns, ISet<Sentence> arg) 
        {
            ISet<Symbol> s = (ISet<Symbol>) arg;
            if (ns.Negated is Symbol) {
                s.Add((Symbol) ns.Negated);
            } else {
                s.UnionWith((ISet<Symbol>) ns.Negated.Accept(this, arg));
                        
            }
            return s;
        }

        public ISet<Symbol> GetNegativeSymbolsIn(Sentence s) {
            return (ISet<Symbol>)s.Accept(this, new HashedSet<Sentence>());
        }
    }

}
