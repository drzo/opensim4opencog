using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing
{
    using Aima.Core.Logic.Common;

    public class PELexer : Lexer 
    {

        ISet<String> connectors;

        public PELexer() {
            connectors = new HashedSet<String>();
            connectors.Add("NOT");
            connectors.Add("AND");
            connectors.Add("OR");
            connectors.Add("=>");
            connectors.Add("<=>");
            connectors.Add("IMPLIES");
            connectors.Add("EQUIV");
        }

        public PELexer(String inputString):this()
        {
            SetInput(inputString);
        }

        public override Token NextToken() {
            if (LookAhead(1) == '(') {
                Consume();
                return new Token(LogicTokenTypes.LParen, "(");

            } else if (LookAhead(1) == ')') {
                Consume();
                return new Token(LogicTokenTypes.RParen, ")");
            } else if (this.IdentifierDetected()) {
                return this.Symbol();

            } else if (char.IsWhiteSpace(LookAhead(1))) 
            {
                Consume();
                return NextToken();
                // return whiteSpace();
            } else if (this.LookAhead(1) == ((char) Int32.Parse("-1"))) 
            {
                return new Token(LogicTokenTypes.EOI, "EOI");
            } else {
                throw new ApplicationException("Lexing error on character " + LookAhead(1));
            }

        }

        private bool IdentifierDetected() {
            return (Util.Util.IdentifierStartRegex.IsMatch(((char)LookAheadBuffer[0]).ToString()))
                    || this.PartOfConnector();
        }

        private bool PartOfConnector() {
            return (LookAhead(1) == '=') || (LookAhead(1) == '<')
                    || (LookAhead(1) == '>');
        }

        private Token Symbol() {
            StringBuilder sbuf = new StringBuilder();
            while ((char.IsLetterOrDigit(LookAhead(1)))
                    || (LookAhead(1) == '=') || (LookAhead(1) == '<')
                    || (LookAhead(1) == '>')) {
                sbuf.Append(LookAhead(1));
                Consume();
            }
            String symbol = sbuf.ToString();
            if (this.IsConnector(symbol)) {
                return new Token(LogicTokenTypes.Connector, sbuf.ToString());
            } else if (symbol.ToLower().Equals("true")) {
                return new Token(LogicTokenTypes.True, "TRUE");
            } else if (symbol.ToLower().Equals("false")) {
                return new Token(LogicTokenTypes.False, "FALSE");
            } else {
                return new Token(LogicTokenTypes.Symbol, sbuf.ToString());
            }

        }

        private Token Connector() {
            StringBuilder sbuf = new StringBuilder();
            while (char.IsLetterOrDigit(LookAhead(1))) {
                sbuf.Append(LookAhead(1));
                Consume();
            }
            return new Token(LogicTokenTypes.Connector, sbuf.ToString());
        }

        private Token WhiteSpace() {
            StringBuilder sbuf = new StringBuilder();
            while (char.IsWhiteSpace(LookAhead(1))) {
                sbuf.Append(LookAhead(1));
                Consume();
            }
            return new Token(LogicTokenTypes.Whitespace, sbuf.ToString());

        }

        private bool IsConnector(String aSymbol) {
            return (connectors.Contains(aSymbol));
        }
    }
}
