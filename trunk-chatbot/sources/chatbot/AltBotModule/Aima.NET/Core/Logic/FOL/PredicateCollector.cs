using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class PredicateCollector : IFOLVisitor 
    {
        public IList<Predicate> GetPredicates(ISentence s) 
        {
            return (IList<Predicate>) s.Accept(this, new List<Predicate>());
        }

        public object VisitPredicate(Predicate p, object arg) {
            IList<Predicate> predicates = (IList<Predicate>) arg;
            predicates.Add(p);
            return predicates;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            return arg;
        }

        public object VisitVariable(Variable variable, object arg) {
            return arg;
        }

        public object VisitConstant(Constant constant, object arg) {
            return arg;
        }

        public object VisitFunction(Function function, object arg) {
            return arg;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
            sentence.Negated.Accept(this, arg);
            return arg;
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            sentence.First.Accept(this, arg);
            sentence.Second.Accept(this, arg);
            return arg;
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {
            sentence.Quantified.Accept(this, arg);
            return arg;
        }
    }
}
