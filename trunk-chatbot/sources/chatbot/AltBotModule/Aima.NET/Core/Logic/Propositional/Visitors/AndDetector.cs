using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class AndDetector : IPLVisitor {

        public object VisitSymbol(Symbol s, ISet<Sentence> arg)
        {
            return false;
        }

        public object VisitTrueSentence(TrueSentence ts, ISet<Sentence> arg)
        {
            return false;
        }

        public object VisitFalseSentence(FalseSentence fs, object arg) {
            return false;
        }

        public object VisitNotSentence(UnarySentence fs, ISet<Sentence> arg)
        {
            return fs.Negated.Accept(this, null);
        }

        public object VisitBinarySentence(BinarySentence fs, ISet<Sentence> arg)
        {
            if (fs.IsAndSentence()) {
                return true;
            } else
            {
                bool first = ((bool)fs.First.Accept(this, null));
                bool second = ((bool) fs.Second.Accept(this, null));
                return first || second;
            }
        }

        public object VisitMultiSentence(MultiSentence fs, ISet<Sentence> arg)
        {
            throw new ApplicationException("can't handle multisentences");
        }

        public bool ContainsEmbeddedAnd(Sentence s) {
            return ((bool) s.Accept(this, null));
        }
    }
}
