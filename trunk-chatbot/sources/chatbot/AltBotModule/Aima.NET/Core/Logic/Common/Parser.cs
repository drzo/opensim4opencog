using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Common
{
    public abstract class Parser
    {

        protected Lexer Lexer { get; set; }

        protected Token[] LookAheadBuffer { get; set; }

        private int lookAheadLimit = 3;

        protected internal int LookAheadLimit
        {
            get
            {
                return lookAheadLimit;
            }
            set
            {
                lookAheadLimit = value;
            }
        }

        public abstract IParseTreeNode Parse(String input);

        protected void FillLookAheadBuffer()
        {
            for (int i = 0; i < this.lookAheadLimit; i++)
            {
                this.LookAheadBuffer[i] = this.Lexer.NextToken();
            }
        }

        protected Token LookAhead(int i)
        {
            return this.LookAheadBuffer[i - 1];
        }

        protected void Consume()
        {
            this.LoadNextTokenFromInput();
        }

        protected void LoadNextTokenFromInput()
        {

            bool eoiEncountered = false;
            for (int i = 0; i < this.lookAheadLimit - 1; i++)
            {

                this.LookAheadBuffer[i] = this.LookAheadBuffer[i + 1];
                if (IsEndOfInput(this.LookAheadBuffer[i]))
                {
                    eoiEncountered = true;
                    break;
                }
            }
            if (!eoiEncountered)
            {
                this.LookAheadBuffer[this.lookAheadLimit - 1] = this.Lexer.NextToken();
            }

        }

        protected bool IsEndOfInput(Token t)
        {
            return t.Type == LogicTokenTypes.EOI;
        }

        protected void Match(String terminalSymbol)
        {
            if (LookAhead(1).Text.Equals(terminalSymbol))
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
    }
}
