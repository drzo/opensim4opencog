using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing
{
    using Aima.Core.Logic.Common;
    using Aima.Core.Logic.FOL.Domain;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class FOLParser
    {
        private FOLLexer lexer;

        protected Token[] lookAheadBuffer;

        protected int lookAhead = 1;

        public FOLParser(FOLLexer lexer)
        {
            this.lexer = lexer;
            lookAheadBuffer = new Token[lookAhead];
        }

        public FOLParser(FOLDomain domain)
            : this(new FOLLexer(domain))
        {
        }

        public FOLDomain GetFOLDomain()
        {
            return lexer.Domain;
        }

        public ISentence Parse(string s)
        {
            this.SetUpToParse(s);
            return this.ParseSentence();
        }

        public void SetUpToParse(string s)
        {
            lexer.Clear();
            lookAheadBuffer = new Token[1];
            lexer.SetInput(s);
            this.FillLookAheadBuffer();
        }

        private ITerm ParseTerm()
        {
            var t = this.LookAhead(1);
            var tokenType = t.Type;
            if (tokenType == LogicTokenTypes.Constant)
            {
                return this.ParseConstant();
            }
            else if (tokenType == LogicTokenTypes.Variable)
            {
                return this.ParseVariable();
            }
            else if (tokenType == LogicTokenTypes.Function)
            {
                return this.ParseFunction();
            }

            else
            {
                return null;
            }
        }

        public ITerm ParseVariable()
        {
            var t = this.LookAhead(1);
            var value = t.Text;
            this.Consume();
            return new Variable(value);
        }

        public ITerm ParseConstant()
        {
            var t = this.LookAhead(1);
            var value = t.Text;
            this.Consume();
            return new Constant(value);
        }

        public ITerm ParseFunction()
        {
            var t = this.LookAhead(1);
            var functionName = t.Text;
            var terms = this.ProcessTerms();
            return new Function(functionName, terms);
        }

        public ISentence ParsePredicate()
        {
            var t = this.LookAhead(1);
            var predicateName = t.Text;
            var terms = this.ProcessTerms();
            return new Predicate(predicateName, terms);
        }

        private IList<ITerm> ProcessTerms()
        {
            this.Consume();
            var terms = new List<ITerm>();
            this.Match("(");
            var term = this.ParseTerm();
            terms.Add(term);

            while (this.LookAhead(1).Type == LogicTokenTypes.Comma)
            {
                this.Match(",");
                term = this.ParseTerm();
                terms.Add(term);
            }
            this.Match(")");
            return terms;
        }

        public ISentence ParseTermEquality()
        {
            var term1 = this.ParseTerm();
            this.Match("=");
            // System.out.println("=");
            var term2 = this.ParseTerm();
            return new TermEquality(term1, term2);
        }

        public ISentence ParseNotSentence()
        {
            this.Match("NOT");
            return new NotSentence(ParseSentence());
        }

        protected Token LookAhead(int i)
        {
            return this.lookAheadBuffer[i - 1];
        }

        protected void Consume()
        {
            // System.out.println("consuming" +lookAheadBuffer[0].getText());
            this.LoadNextTokenFromInput();
            // System.out.println("next token " +lookAheadBuffer[0].getText());
        }

        protected void LoadNextTokenFromInput()
        {

            var eoiEncountered = false;
            for (var i = 0; i < this.lookAhead - 1; i++)
            {

                this.lookAheadBuffer[i] = this.lookAheadBuffer[i + 1];
                if (this.IsEndOfInput(this.lookAheadBuffer[i]))
                {
                    eoiEncountered = true;
                    break;
                }
            }
            if (!eoiEncountered)
            {
                this.lookAheadBuffer[this.lookAhead - 1] = this.lexer.NextToken();
            }

        }

        protected bool IsEndOfInput(Token t)
        {
            return t.Type == LogicTokenTypes.EOI;
        }

        protected void FillLookAheadBuffer()
        {
            for (int i = 0; i < this.lookAhead; i++)
            {
                this.lookAheadBuffer[i] = this.lexer.NextToken();
            }
        }

        protected void Match(string terminalSymbol)
        {
            if (this.LookAhead(1).Text.Equals(terminalSymbol))
            {
                this.Consume();
            }
            else
            {
                throw new ApplicationException(
                        "Syntax error detected at match. Expected "
                                + terminalSymbol + " but got "
                                + LookAhead(1).Text);
            }

        }

        private ISentence ParseSentence()
        {
            Token t = LookAhead(1);
            if (this.LParen(t))
            {
                return this.ParseParanthizedSentence();
            }
            else if ((LookAhead(1).Type == LogicTokenTypes.Quantifier))
            {

                return this.ParseQuantifiedSentence();
            }
            else if (this.NotToken(t))
            {
                return this.ParseNotSentence();
            }
            else if (this.Predicate(t))
            {
                return this.ParsePredicate();
            }
            else if (this.Term(t))
            {
                return this.ParseTermEquality();
            }

            throw new ApplicationException("parse failed with Token " + t.Text);
        }

        private ISentence ParseQuantifiedSentence()
        {
            var quantifier = this.LookAhead(1).Text;
            this.Consume();
            var variables = new List<Variable>();
            var var = (Variable)this.ParseVariable();
            variables.Add(var);
            while (this.LookAhead(1).Type == LogicTokenTypes.Comma)
            {
                this.Consume();
                var = (Variable)this.ParseVariable();
                variables.Add(var);
            }
            var sentence = this.ParseSentence();
            return new QuantifiedSentence(quantifier, variables, sentence);
        }

        private ISentence ParseParanthizedSentence()
        {
            this.Match("(");
            var sen = this.ParseSentence();
            while (BinaryConnector(LookAhead(1)))
            {
                string connector = LookAhead(1).Text;
                this.Consume();
                var other = this.ParseSentence();
                sen = new ConnectedSentence(connector, sen, other);
            }
            this.Match(")");
            return sen; /* new ParanthizedSentence */

        }

        private bool BinaryConnector(Token t)
        {
            return (t.Type == LogicTokenTypes.Connector)
                   && (!t.Text.Equals("NOT"));
        }

        private bool LParen(Token t)
        {
            if (t.Type == LogicTokenTypes.LParen)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        private bool Term(Token t)
        {
            return (t.Type == LogicTokenTypes.Function)
                   || (t.Type == LogicTokenTypes.Constant)
                   || (t.Type == LogicTokenTypes.Variable);
        }

        private bool Predicate(Token t)
        {
            return t.Type == LogicTokenTypes.Predicate;
        }

        private bool NotToken(Token t)
        {
            return (t.Type == LogicTokenTypes.Connector)
                   && t.Text.Equals("NOT");
        }
    }
}
