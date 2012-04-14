using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.Common
{
    public class Token 
    {
        public string Text { get; private set;}

        public int Type { get; private set;}

        public Token(int type, string text) 
        {
            this.Type = type;
            this.Text = text;
        }

        public override string ToString() 
        {
            return String.Format("[ {0} {1} ]", Type, Text );
        }

        public bool Equals(Token other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Text, this.Text) && other.Type == this.Type;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.Text != null ? this.Text.GetHashCode() : 0) * 397) ^ this.Type;
            }
        }
    }
}
