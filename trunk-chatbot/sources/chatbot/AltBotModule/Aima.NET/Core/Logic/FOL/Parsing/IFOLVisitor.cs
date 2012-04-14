using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public interface IFOLVisitor
    {
        object VisitPredicate(Predicate p, object arg);

        object VisitTermEquality(TermEquality equality, object arg);

        object VisitVariable(Variable variable, object arg);

        object VisitConstant(Constant constant, object arg);

        object VisitFunction(Function function, object arg);

        object VisitNotSentence(NotSentence sentence, object arg);

        object VisitConnectedSentence(ConnectedSentence sentence, object arg);

        object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg);
    }
}
