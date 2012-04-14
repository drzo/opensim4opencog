using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class AbstractFOLVisitor : IFOLVisitor 
    {

        //TODO:try to get rid of object types being passed in and returned
        protected ISentence Recreate(object ast) 
        {
            return (ISentence)((ISentence) ast).Copy();
        }

        public virtual object VisitVariable(Variable variable, object arg) 
        {
            return variable.Copy();
        }

        public virtual object VisitQuantifiedSentence(QuantifiedSentence sentence, object arg) 
        {
            var variables = sentence.Variables.Select(var => (Variable)var.Accept(this, arg)).ToList();

            return new QuantifiedSentence(
                sentence.Quantifier, variables, (ISentence)sentence.Quantified.Accept(this, arg));
        }

        public object VisitPredicate(Predicate predicate, object arg) 
        {
            var terms = predicate.Terms;
            var newTerms = terms.Select(t => (ITerm)t.Accept(this, arg)).ToList();
            return new Predicate(predicate.PredicateName, newTerms);

        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            var newTerm1 = (ITerm) equality.Term1.Accept(this, arg);
            var newTerm2 = (ITerm) equality.Term2.Accept(this, arg);
            return new TermEquality(newTerm1, newTerm2);
        }

        public object VisitConstant(Constant constant, object arg) 
        {
            return constant;
        }

        public object VisitFunction(Function function, object arg) 
        {
            var terms = function.Terms;
            var newTerms = terms.Select(t => (ITerm)t.Accept(this, arg)).ToList();
            return new Function(function.GetFunctionName(), newTerms);
        }

        public object VisitNotSentence(NotSentence sentence, object arg)
        {
            return new NotSentence((ISentence)sentence.Negated.Accept(this, arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) 
        {
            var substFirst = (ISentence)sentence.First.Accept(this, arg);
            var substSecond = (ISentence)sentence.Second.Accept(this, arg);
            return new ConnectedSentence(sentence.Connector, substFirst, substSecond);
        }
    }
}
