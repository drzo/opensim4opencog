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

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 345.
    /// 
    /// Every sentence of first-order logic can be converted into an inferentially
    /// equivalent CNF sentence.
    /// 
    /// Note: Transformation rules extracted from 346 and 347, which
    /// are essentially the INSEADO method outlined in:
    /// http://logic.stanford.edu/classes/cs157/2008/lectures/lecture09.pdf
    /// </summary>
    public class CNFConverter 
    {

        private FOLParser parser;
        private SubstVisitor substVisitor;

        public CNFConverter(FOLParser parser) 
        {
            this.parser = parser;

            this.substVisitor = new SubstVisitor();
        }

        public CNF ConvertToCNF(ISentence aSentence) 
        {
            // I)mplications Out:
            ISentence implicationsOut = (ISentence) aSentence.Accept(
                    new ImplicationsOut(), null);

            // N)egations In:
            ISentence negationsIn = (ISentence) implicationsOut.Accept(
                    new NegationsIn(), null);

            // S)tandardize variables:
            // For sentences like:
            // (ForAll x P(x)) V (Exists x Q(x)),
            // which use the same variable name twice, change the name of one of the
            // variables.
            ISentence saQuantifiers = (ISentence) negationsIn.Accept(
                    new StandardizeQuantiferVariables(substVisitor),
                    new HashSet<Variable>());

            // Remove explicit quantifiers, by skolemizing existentials
            // and dropping universals:
            // E)xistentials Out
            // A)lls Out:
            ISentence andsAndOrs = (ISentence) saQuantifiers.Accept(
                    new RemoveQuantifiers(parser), new HashSet<Variable>());

            // D)istribution
            // V over ^:
            ISentence orDistributedOverAnd = (ISentence) andsAndOrs.Accept(
                    new DistributeOrOverAnd(), null);

            // O)perators Out
            return (new CNFConstructor()).Construct(orDistributedOverAnd);
        }
    }

    class ImplicationsOut : IFOLVisitor 
    {
        public object VisitPredicate(Predicate p, object arg) 
        {
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) {
            return variable;
        }

        public object VisitConstant(Constant constant, object arg) {
            return constant;
        }

        public object VisitFunction(Function function, object arg) {
            return function;
        }

        public object VisitNotSentence(NotSentence notSentence, object arg) {
            ISentence negated = notSentence.Negated;

            return new NotSentence((ISentence) negated.Accept(this, arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            ISentence alpha = (ISentence) sentence.First.Accept(this, arg);
            ISentence beta = (ISentence) sentence.Second.Accept(this, arg);

            // Eliminate <=>, bi-conditional elimination,
            // replace (alpha <=> beta) with (~alpha V beta) ^ (alpha V ~beta).
            if (Connectors.IsBiCond(sentence.Connector)) {
                ISentence first = new ConnectedSentence(Connectors.Or,
                        new NotSentence(alpha), beta);
                ISentence second = new ConnectedSentence(Connectors.Or, alpha,
                        new NotSentence(beta));

                return new ConnectedSentence(Connectors.And, first, second);
            }

            // Eliminate =>, implication elimination,
            // replacing (alpha => beta) with (~alpha V beta)
            if (Connectors.IsImplies(sentence.Connector)) {
                return new ConnectedSentence(Connectors.Or, new NotSentence(alpha),
                        beta);
            }

            return new ConnectedSentence(sentence.Connector, alpha, beta);
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {

            return new QuantifiedSentence(sentence.Quantifier, sentence
                    .Variables, (ISentence) sentence.Quantified.Accept(
                    this, arg));
        }
    }

    class NegationsIn : IFOLVisitor {
        public NegationsIn() {

        }

        public object VisitPredicate(Predicate p, object arg) {
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) {
            return variable;
        }

        public object VisitConstant(Constant constant, object arg) {
            return constant;
        }

        public object VisitFunction(Function function, object arg) {
            return function;
        }

        public object VisitNotSentence(NotSentence notSentence, object arg) {
            // CNF requires NOT (~) to appear only in literals, so we 'move ~
            // inwards' by repeated application of the following equivalences:
            ISentence negated = notSentence.Negated;

            // ~(~alpha) equivalent to alpha (double negation elimination)
            if (negated is NotSentence) {
                return ((NotSentence) negated).Negated.Accept(this, arg);
            }

            if (negated is ConnectedSentence) {
                ConnectedSentence negConnected = (ConnectedSentence) negated;
                ISentence alpha = negConnected.First;
                ISentence beta = negConnected.Second;
                // ~(alpha ^ beta) equivalent to (~alpha V ~beta) (De Morgan)
                if (Connectors.IsAnd(negConnected.Connector)) {
                    // I need to ensure the ~s are moved in deeper
                    ISentence notAlpha = (ISentence) (new NotSentence(alpha)).Accept(
                            this, arg);
                    ISentence notBeta = (ISentence) (new NotSentence(beta)).Accept(
                            this, arg);
                    return new ConnectedSentence(Connectors.Or, notAlpha, notBeta);
                }

                // ~(alpha V beta) equivalent to (~alpha ^ ~beta) (De Morgan)
                if (Connectors.IsOr(negConnected.Connector)) {
                    // I need to ensure the ~s are moved in deeper
                    ISentence notAlpha = (ISentence) (new NotSentence(alpha)).Accept(
                            this, arg);
                    ISentence notBeta = (ISentence) (new NotSentence(beta)).Accept(
                            this, arg);
                    return new ConnectedSentence(Connectors.And, notAlpha, notBeta);
                }
            }

            // in addition, rules for negated quantifiers:
            if (negated is QuantifiedSentence) {
                QuantifiedSentence negQuantified = (QuantifiedSentence) negated;
                // I need to ensure the ~ is moved in deeper
                ISentence notP = (ISentence) (new NotSentence(negQuantified
                        .Quantified)).Accept(this, arg);

                // ~ForAll x p becomes Exists x ~p
                if (Quantifiers.IsForall(negQuantified.Quantifier)) {
                    return new QuantifiedSentence(Quantifiers.Exists, negQuantified
                            .Variables, notP);
                }

                // ~Exists x p becomes ForAll x ~p
                if (Quantifiers.IsExists(negQuantified.Quantifier)) {
                    return new QuantifiedSentence(Quantifiers.ForAll, negQuantified
                            .Variables, notP);
                }
            }

            return new NotSentence((ISentence) negated.Accept(this, arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            return new ConnectedSentence(sentence.Connector,
                    (ISentence) sentence.First.Accept(this, arg),
                    (ISentence) sentence.Second.Accept(this, arg));
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {

            return new QuantifiedSentence(sentence.Quantifier, sentence
                    .Variables, (ISentence) sentence.Quantified.Accept(
                    this, arg));
        }
    }

    class StandardizeApartQuantifiedIndexical : IStandardizeApartIndexical 
    {
        private int index;

        public String GetPrefix() 
        {
            return "q";
        }

        public int GetNextIndex() 
        {
            return index++;
        }
    }


    class StandardizeQuantiferVariables : IFOLVisitor {
        // Just use a localized indexical here.
        private StandardizeApartQuantifiedIndexical quantifiedIndexical = new StandardizeApartQuantifiedIndexical();

        private SubstVisitor substVisitor = null;

        public StandardizeQuantiferVariables(SubstVisitor substVisitor) {
            this.substVisitor = substVisitor;
        }

        public object VisitPredicate(Predicate p, object arg) {
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) {
            return variable;
        }

        public object VisitConstant(Constant constant, object arg) {
            return constant;
        }

        public object VisitFunction(Function function, object arg) {
            return function;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
            return new NotSentence((ISentence) sentence.Negated.Accept(this,
                    arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            return new ConnectedSentence(sentence.Connector,
                    (ISentence) sentence.First.Accept(this, arg),
                    (ISentence) sentence.Second.Accept(this, arg));
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence, object arg) 
        {
            ISet<Variable> seenSoFar = (ISet<Variable>) arg;

            // Keep track of what I have to subst locally and
            // what my renamed variables will be.
            IDictionary<Variable, ITerm> localSubst = new Dictionary<Variable, ITerm>();
            IList<Variable> replVariables = new List<Variable>();
            foreach (Variable v in sentence.Variables) 
            {
                // If local variable has be renamed already
                // then I need to come up with own name
                if (seenSoFar.Contains(v)) 
                {
                    var sV = new Variable(quantifiedIndexical.GetPrefix()
                            + quantifiedIndexical.GetNextIndex());
                    localSubst[v] = sV;
                    // Replacement variables should contain new name for variable
                    replVariables.Add(sV);
                } 
                else 
                {
                    // Not already replaced, this name is good
                    replVariables.Add(v);
                }
            }

            // Apply the local subst
            ISentence subst = substVisitor.Subst(localSubst, sentence.Quantified);

            // Ensure all my existing and replaced variable
            // names are tracked
            seenSoFar.UnionWith(replVariables);

            var sQuantified = (ISentence) subst.Accept(this, arg);

            return new QuantifiedSentence(sentence.Quantifier, replVariables,
                    sQuantified);
        }
    }

    class RemoveQuantifiers : IFOLVisitor {

        private FOLParser parser = null;
        private SubstVisitor substVisitor = null;

        public RemoveQuantifiers(FOLParser parser) {
            this.parser = parser;

            substVisitor = new SubstVisitor();
        }

        public object VisitPredicate(Predicate p, object arg) {
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) {
            return variable;
        }

        public object VisitConstant(Constant constant, object arg) {
            return constant;
        }

        public object VisitFunction(Function function, object arg) {
            return function;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
            return new NotSentence((ISentence) sentence.Negated.Accept(this,
                    arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            return new ConnectedSentence(sentence.Connector,
                    (ISentence) sentence.First.Accept(this, arg),
                    (ISentence) sentence.Second.Accept(this, arg));
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {
            ISentence quantified = sentence.Quantified;
            var universalScope = (ISet<Variable>) arg;

            // Skolemize: Skolemization is the process of removing existential
            // quantifiers by elimination. This is done by introducing Skolem
            // functions. The general rule is that the arguments of the Skolem
            // function are all the universally quantified variables in whose
            // scope the existential quantifier appears.
            if (Quantifiers.IsExists(sentence.Quantifier)) {
                IDictionary<Variable, ITerm> skolemSubst = new Dictionary<Variable, ITerm>();
                foreach (Variable eVar in sentence.Variables) 
                {
                    if (universalScope.Count > 0) 
                    {
                        // Replace with a Skolem Function
                        var skolemFunctionName = parser.GetFOLDomain()
                                .AddSkolemFunction();
                        skolemSubst[eVar] = new Function(skolemFunctionName, new List<ITerm>((IEnumerable <ITerm>)universalScope));
                    } 
                    else 
                    {
                        // Replace with a Skolem Constant
                        String skolemConstantName = parser.GetFOLDomain()
                                .AddSkolemConstant();
                        skolemSubst[eVar] = new Constant(skolemConstantName);
                    }
                }

                ISentence skolemized = substVisitor.Subst(skolemSubst, quantified);
                return skolemized.Accept(this, arg);
            }

            // Drop universal quantifiers.
            if (Quantifiers.IsForall(sentence.Quantifier)) 
            {
                // Add to the universal scope so that
                // existential skolemization may be done correctly
                universalScope.UnionWith(sentence.Variables);

                ISentence droppedUniversal = (ISentence) quantified.Accept(this, arg);

                // Enusre my scope is removed before moving back up
                // the call stack when returning
                foreach(var v in sentence.Variables)
                {
                    universalScope.Remove(v);
                }
                
                return droppedUniversal;
            }

            // Should not reach here as have already
            // handled the two quantifiers.
            throw new InvalidOperationException("Unhandled Quantifier:" + sentence.Quantifier);
        }
    }

    class DistributeOrOverAnd : IFOLVisitor 
    {

        public object VisitPredicate(Predicate p, object arg) 
        {
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) 
        {
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) 
        {
            return variable;
        }

        public object VisitConstant(Constant constant, object arg) 
        {
            return constant;
        }

        public object VisitFunction(Function function, object arg) 
        {
            return function;
        }

        public object VisitNotSentence(NotSentence sentence, object arg) 
        {
            return new NotSentence((ISentence) sentence.Negated.Accept(this, arg));
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) 
        {
            // Distribute V over ^:

            // This will cause flattening out of nested ^s and Vs
            var alpha = (ISentence) sentence.First.Accept(this, arg);
            var beta = (ISentence) sentence.Second.Accept(this, arg);

            // (alpha V (beta ^ gamma)) equivalent to
            // ((alpha V beta) ^ (alpha V gamma))
            if (Connectors.IsOr(sentence.Connector) && beta is ConnectedSentence) 
            {
                var betaAndGamma = (ConnectedSentence) beta;
                if (Connectors.IsAnd(betaAndGamma.Connector)) 
                {
                    beta = betaAndGamma.First;
                    var gamma = betaAndGamma.Second;
                    return new ConnectedSentence(Connectors.And,
                            (ISentence) (new ConnectedSentence(Connectors.Or, alpha, beta)).Accept(this, arg),
                            (ISentence) (new ConnectedSentence(Connectors.Or, alpha, gamma)).Accept(this, arg));
                }
            }

            // ((alpha ^ gamma) V beta) equivalent to
            // ((alpha V beta) ^ (gamma V beta))
            if (Connectors.IsOr(sentence.Connector) && alpha is ConnectedSentence)
            {
                var alphaAndGamma = (ConnectedSentence)alpha;
                if (Connectors.IsAnd(alphaAndGamma.Connector))
                {
                    alpha = alphaAndGamma.First;
                    var gamma = alphaAndGamma.Second;
                    return new ConnectedSentence(
                        Connectors.And,
                        (ISentence)(new ConnectedSentence(Connectors.Or, alpha, beta)).Accept(this, arg),
                        (ISentence)(new ConnectedSentence(Connectors.Or, gamma, beta)).Accept(this, arg));
                }
            }

            return new ConnectedSentence(sentence.Connector, alpha, beta);
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence, object arg) {
            // This should not be called as should have already
            // removed all of the quantifiers.
            throw new InvalidOperationException("All quantified sentences should have already been removed.");
        }
    }

    class CNFConstructor : IFOLVisitor 
    {
        public CNF Construct(ISentence orDistributedOverAnd)
        {
            var ad = new ArgData();

            orDistributedOverAnd.Accept(this, ad);

            return new CNF(ad.Clauses);
        }

        public object VisitPredicate(Predicate p, object arg) {
            ArgData ad = (ArgData) arg;
            if (ad.Negated) 
            {
                ad.Clauses[ad.Clauses.Count - 1].AddNegativeLiteral(p);
            } 
            else 
            {
                ad.Clauses[ad.Clauses.Count - 1].AddPositiveLiteral(p);
            }
            return p;
        }

        public object VisitTermEquality(TermEquality equality, object arg) {
            ArgData ad = (ArgData) arg;
            if (ad.Negated) {
                ad.Clauses[ad.Clauses.Count - 1].AddNegativeLiteral(equality);
            } else {
                ad.Clauses[ad.Clauses.Count - 1].AddPositiveLiteral(equality);
            }
            return equality;
        }

        public object VisitVariable(Variable variable, object arg) {
            // This should not be called
            throw new InvalidOperationException("VisitVariable() should not be called.");
        }

        public object VisitConstant(Constant constant, object arg) {
            // This should not be called
            throw new InvalidOperationException("VisitConstant() should not be called.");
        }

        public object VisitFunction(Function function, object arg) {
            // This should not be called
            throw new InvalidOperationException("VisitFunction() should not be called.");
        }

        public object VisitNotSentence(NotSentence sentence, object arg) {
            ArgData ad = (ArgData) arg;
            // Indicate that the enclosed predicate is negated
            ad.Negated = true;
            sentence.Negated.Accept(this, arg);
            ad.Negated = false;

            return sentence;
        }

        public object VisitConnectedSentence(ConnectedSentence sentence, object arg) {
            ArgData ad = (ArgData) arg;
            ISentence first = sentence.First;
            ISentence second = sentence.Second;

            first.Accept(this, arg);
            if (Connectors.IsAnd(sentence.Connector)) {
                ad.Clauses.Add(new Clause());
            }
            second.Accept(this, arg);

            return sentence;
        }

        public object VisitQuantifiedSentence(QuantifiedSentence sentence,
                object arg) {
            // This should not be called as should have already
            // removed all of the quantifiers.
            throw new InvalidOperationException(
                    "All quantified sentences should have already been removed.");
        }

        class ArgData {
            public IList<Clause> Clauses = new List<Clause>();
            public bool Negated = false;

            public ArgData() {
                this.Clauses.Add(new Clause());
            }
        }
    }
}
