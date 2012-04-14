using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;
    using Iesi.Collections.Generic;

    /// <summary>
    /// Abstract base class for Demodulation and Paramodulation algorithms.
    /// </summary>
    public abstract class AbstractModulation 
    {
        protected VariableCollector variableCollector = new VariableCollector();
        protected Unifier unifier = new Unifier();
        protected SubstVisitor substVisitor = new SubstVisitor();

        protected abstract bool IsValidMatch(ITerm toMatch,
                ISet<Variable> toMatchVariables, ITerm possibleMatch,
                IDictionary<Variable, ITerm> substitution);

        protected IdentifyCandidateMatchingTerm GetMatchingSubstitution(
                ITerm toMatch, IAtomicSentence expression) 
        {

            IdentifyCandidateMatchingTerm icm = new IdentifyCandidateMatchingTerm(toMatch, expression, this);

            if (icm.IsMatch()) 
            {
                return icm;
            }

            // indicates no match
            return null;
        }

        protected class IdentifyCandidateMatchingTerm : IFOLVisitor 
        {
            private ITerm toMatch = null;
            private ISet<Variable> toMatchVariables = null;
            private ITerm matchingTerm = null;
            private IDictionary<Variable, ITerm> substitution = null;

            private AbstractModulation parent;

            public IdentifyCandidateMatchingTerm(ITerm toMatch,
                    IAtomicSentence expression, AbstractModulation p) 
            {
                this.toMatch = toMatch;
                this.toMatchVariables = p.variableCollector
                        .CollectAllVariables(toMatch);
                this.parent = p;

                expression.Accept(this, null);
            }

            public bool IsMatch() 
            {
                return null != matchingTerm;
            }

            public ITerm GetMatchingTerm() 
            {
                return matchingTerm;
            }

            public IDictionary<Variable, ITerm> GetMatchingSubstitution() 
            {
                return substitution;
            }

            public object VisitPredicate(Predicate p, object arg) 
            {
                foreach (ITerm t in p.GetArgs()) 
                {
                    // Finish processing if have found a match
                    if (null != matchingTerm) 
                    {
                        break;
                    }
                    t.Accept(this, null);
                }
                return p;
            }

            public object VisitTermEquality(TermEquality equality, object arg) 
            {
                foreach (ITerm t in equality.GetArgs()) 
                {
                    // Finish processing if have found a match
                    if (null != matchingTerm) 
                    {
                        break;
                    }
                    t.Accept(this, null);
                }
                return equality;
            }

            public object VisitVariable(Variable variable, object arg) 
            {

                if (null != (substitution = parent.unifier.Unify(toMatch, variable))) 
                {
                    if (parent.IsValidMatch(toMatch, toMatchVariables, variable,
                            substitution)) 
                    {
                        matchingTerm = variable;
                    }
                }

                return variable;
            }

            public object VisitConstant(Constant constant, object arg)
            {
                if (null != (substitution = parent.unifier.Unify(toMatch, constant)))
                {
                    if (parent.IsValidMatch(toMatch, toMatchVariables, constant,
                            substitution)) {
                        matchingTerm = constant;
                    }
                }

                return constant;
            }

            public object VisitFunction(Function function, object arg) 
            {
                if (null != (substitution = parent.unifier.Unify(toMatch, function))) 
                {
                    if (parent.IsValidMatch(toMatch, toMatchVariables, function,
                            substitution)) {
                        matchingTerm = function;
                    }
                }

                if (null == matchingTerm) {
                    // Try the Function's arguments
                    foreach (ITerm t in function.GetArgs()) 
                    {
                        // Finish processing if have found a match
                        if (null != matchingTerm) {
                            break;
                        }
                        t.Accept(this, null);
                    }
                }

                return function;
            }

            public object VisitNotSentence(NotSentence sentence, object arg) {
                throw new InvalidOperationException(
                        "visitNotSentence() should not be called.");
            }

            public object VisitConnectedSentence(ConnectedSentence sentence,
                    object arg) {
                throw new InvalidOperationException(
                        "visitConnectedSentence() should not be called.");
            }

            public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                    object arg) {
                throw new InvalidOperationException(
                        "visitQuantifiedSentence() should not be called.");
            }
        }

        protected class ReplaceMatchingTerm : IFOLVisitor 
        {
            private ITerm toReplace = null;
            private ITerm replaceWith = null;
            private bool replaced = false;

            public IAtomicSentence Replace(IAtomicSentence expression,
                    ITerm toReplace, ITerm replaceWith)
            {
                this.toReplace = toReplace;
                this.replaceWith = replaceWith;

                return (IAtomicSentence) expression.Accept(this, null);
            }

            //
            // START-FOLVisitor
            public object VisitPredicate(Predicate p, object arg)
            {
                IList<ITerm> newTerms = new List<ITerm>();
                foreach (ITerm t in p.Terms) 
                {
                    ITerm subsTerm = (ITerm) t.Accept(this, arg);
                    newTerms.Add(subsTerm);
                }
                return new Predicate(p.PredicateName, newTerms);
            }

            public object VisitTermEquality(TermEquality equality, object arg)
            {
                ITerm newTerm1 = (ITerm) equality.Term1.Accept(this, arg);
                ITerm newTerm2 = (ITerm) equality.Term2.Accept(this, arg);
                return new TermEquality(newTerm1, newTerm2);
            }

            public object VisitVariable(Variable variable, object arg)
            {
                if (!replaced)
                {
                    if (toReplace.Equals(variable))
                    {
                        replaced = true;
                        return replaceWith;
                    }
                }
                return variable;
            }

            public object VisitConstant(Constant constant, object arg)
            {
                if (!replaced)
                {
                    if (toReplace.Equals(constant)) 
                    {
                        replaced = true;
                        return replaceWith;
                    }
                }
                return constant;
            }

            public object VisitFunction(Function function, object arg) 
            {
                if (!replaced) 
                {
                    if (toReplace.Equals(function))
                    {
                        replaced = true;
                        return replaceWith;
                    }
                }

                IList<ITerm> newTerms = new List<ITerm>();
                foreach (ITerm t in function.Terms)
                {
                    ITerm subsTerm = (ITerm) t.Accept(this, arg);
                    newTerms.Add(subsTerm);
                }
                return new Function(function.GetFunctionName(), newTerms);
            }

            public object VisitNotSentence(NotSentence sentence, object arg) {
                throw new InvalidOperationException(
                        "visitNotSentence() should not be called.");
            }

            public object VisitConnectedSentence(ConnectedSentence sentence,
                    object arg) {
                throw new InvalidOperationException(
                        "visitConnectedSentence() should not be called.");
            }

            public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                    object arg) {
                throw new InvalidOperationException(
                        "visitQuantifiedSentence() should not be called.");
            }
        }
    }

}
