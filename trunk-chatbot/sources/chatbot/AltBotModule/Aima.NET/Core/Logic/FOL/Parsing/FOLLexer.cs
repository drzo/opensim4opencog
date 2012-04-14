using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.FOL.Parsing
{
    using System.Text.RegularExpressions;

    using Aima.Core.Logic.Common;
    using Aima.Core.Logic.FOL.Domain;

    public class FOLLexer : Lexer 
    {
        public FOLDomain Domain { get; private set; }
        private ISet<string> connectors, quantifiers;

        public FOLLexer(FOLDomain domain) 
        {
            this.Domain = domain;

            connectors = new HashedSet<string>();
            connectors.Add(Connectors.Not);
            connectors.Add(Connectors.And);
            connectors.Add(Connectors.Or);
            connectors.Add(Connectors.Implies);
            connectors.Add(Connectors.BiCond);

            quantifiers = new HashedSet<string>();
            quantifiers.Add(Quantifiers.ForAll);
            quantifiers.Add(Quantifiers.Exists);
        }

        public override Token NextToken() 
        {
            if (LookAhead(1) == '(') 
            {
                Consume();
                return new Token(LogicTokenTypes.LParen, "(");

            }
            else if (LookAhead(1) == ')')
            {
                Consume();
                return new Token(LogicTokenTypes.RParen, ")");

            }
            else if (LookAhead(1) == ',')
            {
                Consume();
                return new Token(LogicTokenTypes.Comma, ",");

            } else if (this.IdentifierDetected()) {
                // System.out.println("identifier detected");
                return this.Identifier();
            }
            else if (char.IsWhiteSpace(LookAhead(1)))
            {
                Consume();
                return NextToken();
            }
            else if (this.LookAhead(1) == ((char)Int32.Parse("-1"))) 
            {
                return new Token(LogicTokenTypes.EOI, "EOI");
            } else {
                throw new ApplicationException("Lexing error on character "+ LookAhead(1));
            }
        }

        private Token Identifier() 
        {
            var sbuf = new StringBuilder();
            
            //TODO: verify that this is ok replacement for Character.isJavaIdentifierPart

            while ((Util.Util.IdentifierPartRegex.IsMatch(LookAhead(1).ToString()))|| this.PartOfConnector()) {
                sbuf.Append(LookAhead(1));
                Consume();
            }
            string readString = sbuf.ToString();
            // System.out.println(readString);)
            if (connectors.Contains(readString)) 
            {
                return new Token(LogicTokenTypes.Connector, readString);
            } 
            else if (quantifiers.Contains(readString)) 
            {
                return new Token(LogicTokenTypes.Quantifier, readString);
            } 
            else if (Domain.Predicates.Contains(readString)) 
            {
                return new Token(LogicTokenTypes.Predicate, readString);
            } 
            else if (Domain.Functions.Contains(readString)) 
            {
                return new Token(LogicTokenTypes.Function, readString);
            }
            else if (Domain.Constants.Contains(readString))
            {
                return new Token(LogicTokenTypes.Constant, readString);
            } else if (this.IsVariable(readString)) {
                return new Token(LogicTokenTypes.Variable, readString);
            } else if (readString.Equals("=")) {
                return new Token(LogicTokenTypes.LEquals, readString);
            } else {
                throw new ApplicationException("Lexing error on character "+ LookAhead(1));
            }

        }

        private bool IsVariable(string s) {
            return (char.IsLower(s.ToCharArray()[0]));
        }

        private bool IdentifierDetected() 
        {
            return (Util.Util.IdentifierStartRegex.IsMatch(((char)LookAheadBuffer[0]).ToString()))
                    || this.PartOfConnector();
        }

        private bool PartOfConnector() 
        {
            return (LookAhead(1) == '=') || (LookAhead(1) == '<')
                    || (LookAhead(1) == '>');
        }
    }
}
