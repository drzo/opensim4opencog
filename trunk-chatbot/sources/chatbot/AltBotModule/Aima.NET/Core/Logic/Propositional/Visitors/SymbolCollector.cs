using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class SymbolCollector : BasicTraverser {

        public override object VisitSymbol(Symbol s, ISet<Sentence> arg) 
        {
            arg.Add(new Symbol(s.Value));
            return arg;
        }

        public ISet<Symbol> GetSymbolsIn(Sentence s) {
            if (s == null) {// empty knowledge bases == null fix this later
                return new HashedSet<Symbol>();
            }

            var ret = new HashedSet<Symbol>();
            foreach (var part in (ISet<Sentence>)s.Accept(this, new HashedSet<Sentence>()))
            {
                if (part is Symbol)
                {
                    ret.Add((Symbol)part);
                }
            }
            return ret;
        }
    }
}
