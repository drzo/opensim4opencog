using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL
{
    using Aima.Core.Logic.FOL.KB.Data;
    using Aima.Core.Logic.FOL.Parsing;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class StandardizeApartInPlace 
    {
        private static CollectAllVariables _collectAllVariables = new CollectAllVariables();

        public static int StandardizeApart(Chain c, int saIdx) 
        {
            IList<Variable> variables = new List<Variable>();
            foreach (Literal l in c.GetLiterals()) {
                CollectAllVariables(l.AtomicSentence, variables);
            }

            return StandardizeApart(variables, c, saIdx);
        }

        public static int StandardizeApart(Clause c, int saIdx) {
            IList<Variable> variables = new List<Variable>();
            foreach (Literal l in c.GetLiterals()) {
                CollectAllVariables(l.AtomicSentence, variables);
            }

            return StandardizeApart(variables, c, saIdx);
        }

        private static int StandardizeApart(IList<Variable> variables, object expr,
                int saIdx) {
            IDictionary<string, int> indexicals = new Dictionary<string, int>();
            foreach (Variable v in variables) {
                if (!indexicals.ContainsKey(v.GetIndexedValue())) {
                    indexicals[v.GetIndexedValue()] = saIdx++;
                }
            }
            foreach (Variable v in variables) {
                int i = indexicals[v.GetIndexedValue()];
                
                //TODO: Below condition doesn't seem to make sense even in java version. Remove?
                //if (null == i) 
                //{
                //    throw new RuntimeException("ERROR: duplicate var=" + v
                //            + ", expr=" + expr);
                //} 
                //else 
                //{
                v.Indexical = i;
                //}
            }

            return saIdx;
        }

        private static void CollectAllVariables(ISentence s, IList<Variable> vars) {
            s.Accept(_collectAllVariables, vars);
        }
    }

    class CollectAllVariables : IFOLVisitor 
    {
        public object VisitVariable(Variable var, object arg) {
            var variables = (IList<Variable>) arg;
            variables.Add(var);
            return var;
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {
            // Ensure I collect quantified variables too
            var variables = (IList<Variable>) arg;

            foreach (var v in sentence.Variables)
            {
                variables.Add(v);
            }

            sentence.Quantified.Accept(this, arg);

            return sentence;
        }

        public object VisitPredicate(Predicate predicate, object arg) {
            foreach (ITerm t in predicate.Terms) {
                t.Accept(this, arg);
            }
            return predicate;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            equality.Term1.Accept(this, arg);
            equality.Term2.Accept(this, arg);
            return equality;
        }

        public object VisitConstant(Constant constant, object arg) {
            return constant;
        }

        public object VisitFunction(Function function, object arg) {
            foreach (ITerm t in function.Terms) {
                t.Accept(this, arg);
            }
            return function;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
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
