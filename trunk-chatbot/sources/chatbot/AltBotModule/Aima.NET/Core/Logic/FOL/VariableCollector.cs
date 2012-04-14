using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class VariableCollector : IFOLVisitor 
    {
        // Note: The set guarantees the order in which they were
        // found.
        public ISet<Variable> CollectAllVariables(ISentence sentence) 
        {
            ISet<Variable> variables = new HashedSet<Variable>();

            sentence.Accept(this, variables);

            return variables;
        }

        public ISet<Variable> CollectAllVariables(ITerm aTerm) 
        {
            ISet<Variable> variables = new HashedSet<Variable>();

            aTerm.Accept(this, variables);

            return variables;
        }

        public ISet<Variable> CollectAllVariables(Clause aClause) 
        {
            ISet<Variable> variables = new HashedSet<Variable>();

            foreach (Literal l in aClause.GetLiterals()) 
            {
                l.AtomicSentence.Accept(this, variables);
            }

            return variables;
        }

        public ISet<Variable> CollectAllVariables(Chain aChain) 
        {
            ISet<Variable> variables = new HashedSet<Variable>();

            foreach (Literal l in aChain.GetLiterals()) 
            {
                l.AtomicSentence.Accept(this, variables);
            }

            return variables;
        }

        public object VisitVariable(Variable var, object arg) 
        {
            var variables = (ISet<Variable>) arg;
            variables.Add(var);
            return var;
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence, object arg) 
        {
            // Ensure I collect quantified variables too
            var variables = (ISet<Variable>) arg;
            variables.UnionWith(sentence.Variables);

            sentence.Quantified.Accept(this, arg);

            return sentence;
        }

        public object VisitPredicate(Predicate predicate, object arg) 
        {
            foreach (var t in predicate.Terms) 
            {
                t.Accept(this, arg);
            }
            return predicate;
        }

        public object VisitTermEquality(TermEquality equality, object arg) 
        {
            equality.Term1.Accept(this, arg);
            equality.Term2.Accept(this, arg);
            return equality;
        }

        public object VisitConstant(Constant constant, object arg) 
        {
            return constant;
        }

        public object VisitFunction(Function function, object arg) 
        {
            foreach (var t in function.Terms) 
            {
                t.Accept(this, arg);
            }

            return function;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) 
        {
            sentence.Negated.Accept(this, arg);
            return sentence;
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) 
        {
            sentence.First.Accept(this, arg);
            sentence.Second.Accept(this, arg);
            return sentence;
        }
    }
}
