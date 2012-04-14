using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    public class FalseSentence : AtomicSentence 
    {
        public override string ToString() {
            return "FALSE";
        }

        public override object Accept(IPLVisitor plv, ISet<Sentence> arg) {
            return plv.VisitFalseSentence(this, arg);
        }
    }
}
