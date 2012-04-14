using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing
{
    using Aima.Core.Logic.Common;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public interface IPLVisitor : IVisitor {
        object VisitSymbol(Symbol s, ISet<Sentence> arg);

        object VisitTrueSentence(TrueSentence ts, ISet<Sentence> arg);

        object VisitFalseSentence(FalseSentence fs, object arg);

        object VisitNotSentence(UnarySentence fs, ISet<Sentence> arg);

        object VisitBinarySentence(BinarySentence fs, ISet<Sentence> arg);

        object VisitMultiSentence(MultiSentence fs, ISet<Sentence> arg);
    }
}
