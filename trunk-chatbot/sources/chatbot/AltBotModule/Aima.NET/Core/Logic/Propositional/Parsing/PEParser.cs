using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Propositional.Parsing
{
    using Aima.Core.Logic.Common;
    using Aima.Core.Logic.Propositional.Parsing.AST;

    public class PEParser : Parser 
    {
        public PEParser() {
            LookAheadBuffer = new Token[LookAheadLimit];
        }

        public override IParseTreeNode Parse(string inputString) {
            Lexer = new PELexer(inputString);
            FillLookAheadBuffer();
            return this.ParseSentence();
        }

        private TrueSentence ParseTrue() {
            Consume();
            return new TrueSentence();
        }

        private FalseSentence ParseFalse() {
            Consume();
            return new FalseSentence();
        }

        private Symbol ParseSymbol() {
            string sym = LookAhead(1).Text;
            Consume();
            return new Symbol(sym);
        }

        private AtomicSentence ParseAtomicSentence() {
            Token t = LookAhead(1);
            if (t.Type == LogicTokenTypes.True) {
                return this.ParseTrue();
            } else if (t.Type == LogicTokenTypes.False) {
                return this.ParseFalse();
            } else if (t.Type == LogicTokenTypes.Symbol) {
                return this.ParseSymbol();
            } else {
                throw new ApplicationException(
                        "Error in parseAtomicSentence with Token " + LookAhead(1));
            }
        }

        private UnarySentence ParseNotSentence() {
            Match("NOT");
            Sentence sen = this.ParseSentence();
            return new UnarySentence(sen);
        }

        private MultiSentence ParseMultiSentence() {
            Consume();
            string connector = LookAhead(1).Text;
            Consume();
            IList<Sentence> sentences = new List<Sentence>();
            while (LookAhead(1).Type != LogicTokenTypes.RParen) {
                Sentence sen = this.ParseSentence();
                // Consume();
                sentences.Add(sen);
            }
            Match(")");
            return new MultiSentence(connector, sentences);
        }

        private Sentence ParseSentence() {
            if (this.DetectAtomicSentence()) {
                return this.ParseAtomicSentence();
            } else if (this.DetectBracket()) {
                return this.ParseBracketedSentence();
            } else if (this.DetectNot()) {
                return this.ParseNotSentence();
            } else {

                throw new ApplicationException("Parser Error Token = " + LookAhead(1));
            }
        }

        private bool DetectNot() {
            return (LookAhead(1).Type == LogicTokenTypes.Connector)
                    && (LookAhead(1).Text.Equals("NOT"));
        }

        private Sentence ParseBracketedSentence() {

            if (this.DetectMultiOperator()) {
                return this.ParseMultiSentence();
            } else {
                Match("(");
                Sentence one = this.ParseSentence();
                if (LookAhead(1).Type == LogicTokenTypes.RParen) {
                    Match(")");
                    return one;
                } else if ((LookAhead(1).Type == LogicTokenTypes.Connector)
                        && (!(LookAhead(1).Text.Equals("Not")))) 
                {
                    string connector = LookAhead(1).Text;
                    Consume(); // connector
                    Sentence two = this.ParseSentence();
                    Match(")");
                    return new BinarySentence(connector, one, two);
                }

            }
            throw new ApplicationException(
                    " Runtime Exception at Bracketed Expression with token "
                            + LookAhead(1));
        }

        private bool DetectMultiOperator() {
            return (LookAhead(1).Type == LogicTokenTypes.LParen)
                    && ((LookAhead(2).Text.Equals("AND")) || (LookAhead(2)
                            .Text.Equals("OR")));
        }

        private bool DetectBracket() {
            return LookAhead(1).Type == LogicTokenTypes.LParen;
        }

        private bool DetectAtomicSentence() {
            int type = LookAhead(1).Type;
            return (type == LogicTokenTypes.True)
                    || (type == LogicTokenTypes.False)
                    || (type == LogicTokenTypes.Symbol);
        }
    }
}
