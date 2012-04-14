using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Common
{
    using System.IO;

    public abstract class Lexer
    {
        public abstract Token NextToken();

        protected TextReader Input { get; private set; }

        protected internal int LookAheadIndex
        {
            get
            {
                return this.lookAheadIndex;
            }
        }

        private int lookAheadIndex = 1;

        protected int[] LookAheadBuffer { get; private set; }

        public void SetInput(string inputString)
        {
            this.LookAheadBuffer = new int[lookAheadIndex];
            this.Input = new StringReader(inputString);
            this.FillLookAheadBuffer();
        }

        public void Clear()
        {
            this.Input = null;
            this.LookAheadBuffer = null;
        }

        protected void FillLookAheadBuffer()
        {
            LookAheadBuffer[0] = (char)Input.Read();
        }

        protected char LookAhead(int position)
        {
            return (char)LookAheadBuffer[position - 1];

        }

        protected bool IsEndOfFile(int i)
        {
            return (i == -1);
        }

        protected void LoadNextCharacterFromInput()
        {

            bool eofEncountered = false;
            for (int i = 0; i < this.LookAheadIndex - 1; i++)
            {

                this.LookAheadBuffer[i] = this.LookAheadBuffer[i + 1];
                if (this.IsEndOfFile(this.LookAheadBuffer[i]))
                {
                    eofEncountered = true;
                    break;
                }
            }
            if (!eofEncountered)
            {
                LookAheadBuffer[this.LookAheadIndex - 1] = Input.Read();
            }

        }

        protected void Consume()
        {
            this.LoadNextCharacterFromInput();
        }
    }
}
