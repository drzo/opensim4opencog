using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;


namespace Aima.Core.Logic.Propositional.Visitors
{
    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    /// <summary>
    /// Super class of Visitors that are "read only" and gather information from an
    /// existing parse tree .
    /// </summary>
    public class BasicTraverser : IPLVisitor {

        public virtual object VisitSymbol(Symbol s, ISet<Sentence> arg) {
            return arg;
        }

        public object VisitTrueSentence(TrueSentence ts, ISet<Sentence> arg)
        {
            return arg;
        }

        public object VisitFalseSentence(FalseSentence fs, object arg) {
            return arg;
        }

        public virtual object VisitNotSentence(UnarySentence ns, ISet<Sentence> arg) {
            return new HashSet<Sentence>(arg.Union((ISet<Sentence>) ns.Negated.Accept(this, arg)));
        }

        public virtual object VisitBinarySentence(BinarySentence bs, ISet<Sentence> arg)
        {
            var termunion =
                new HashSet<Sentence>((ISet<Sentence>)bs.First.Accept(this, arg)).Union(
                    (ISet<Sentence>)bs.Second.Accept(this, arg));
            var ret = new HashSet<Sentence>(arg);
            ret.UnionWith(termunion);
            return ret;
        }

        public object VisitMultiSentence(MultiSentence fs, ISet<Sentence> arg)
        {
            throw new ApplicationException("Can't handle MultiSentence");
        }
    }

}
