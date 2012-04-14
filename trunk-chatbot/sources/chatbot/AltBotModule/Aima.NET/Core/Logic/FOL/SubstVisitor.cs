using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class SubstVisitor : AbstractFOLVisitor 
    {
        /// <summary>
        /// Note: Refer to Artificial Intelligence A Modern Approach (3rd Edition): page 323 
        /// </summary>
        /// <param name="theta">a substitution.</param>
        /// <param name="aSentence">the substitution has been applied to.</param>
        /// <returns>a new Sentence representing the result of applying the substitution theta to aSentence.</returns>
        public ISentence Subst(IDictionary<Variable, ITerm> theta, ISentence aSentence) 
        {
            return (ISentence) aSentence.Accept(this, theta);
        }

        public ITerm Subst(IDictionary<Variable, ITerm> theta, ITerm aTerm) 
        {
            return (ITerm) aTerm.Accept(this, theta);
        }

        public Function Subst(IDictionary<Variable, ITerm> theta, Function aFunction) 
        {
            return (Function) aFunction.Accept(this, theta);
        }

        public Literal Subst(IDictionary<Variable, ITerm> theta, Literal aLiteral) 
        {
            return aLiteral.NewInstance((IAtomicSentence) aLiteral.AtomicSentence.Accept(this, theta));
        }

        public override object VisitVariable(Variable variable, object arg) 
        {
            IDictionary<Variable, ITerm> substitution = (IDictionary<Variable, ITerm>) arg;
            if (substitution.ContainsKey(variable)) 
            {
                return substitution[variable].Copy();
            }
            return variable.Copy();
        }

        public override object VisitQuantifiedSentence(QuantifiedSentence sentence, object arg) 
        {
            var substitution = (IDictionary<Variable, ITerm>) arg;

            var quantified = sentence.Quantified;
            var quantifiedAfterSubs = (ISentence)quantified.Accept(this, arg);

            var variables = new List<Variable>();
            foreach (Variable v in sentence.Variables)
            {
                if (substitution.ContainsKey(v))
                {
                    ITerm st = substitution[v];
                    if (st is Variable)
                    {
                        // Only if it is a variable to I replace it, otherwise
                        // I drop it.
                        variables.Add((Variable)st.Copy());
                    }
                }
                else
                {
                    // No substitution for the quantified variable, so
                    // keep it.
                    variables.Add((Variable) v.Copy());
                }
            }

            // If not variables remaining on the quantifier, then drop it
            if (variables.Count == 0) 
            {
                return quantifiedAfterSubs;
            }

            return new QuantifiedSentence(sentence.Quantifier, variables, quantifiedAfterSubs);
        }
    }
}
