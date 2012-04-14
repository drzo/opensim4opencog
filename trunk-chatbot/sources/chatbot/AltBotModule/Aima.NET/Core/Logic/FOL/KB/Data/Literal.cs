using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.KB.Data
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    /// <summary>
    ///Artificial Intelligence A Modern Approach (3rd Edition): page 244.
    /// 
    /// A literal is either an atomic sentence (a positive literal) or
    /// a negated atomic sentence (a negative literal). 
    /// </summary>
    public class Literal 
    {
        public IAtomicSentence AtomicSentence { get; private set; }
        private bool negativeLiteral;

        public Literal(IAtomicSentence atomicSentence) 
        {
            this.AtomicSentence = atomicSentence;
        }

        public Literal(IAtomicSentence atomicSentence, bool negated) 
        {
            this.AtomicSentence = atomicSentence;
            this.negativeLiteral = negated;
        }

        public virtual Literal NewInstance(IAtomicSentence atomSentence) 
        {
            return new Literal(atomSentence, this.negativeLiteral);
        }

        public bool IsPositiveLiteral() 
        {
            return !this.negativeLiteral;
        }

        public bool IsNegativeLiteral() 
        {
            return this.negativeLiteral;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            if (this.IsNegativeLiteral())
            {
                sb.Append("~");
            }
            sb.Append(this.AtomicSentence.ToString());

            return sb.ToString();
        }

        public override bool Equals(object o) {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            if (o.GetType() != typeof(Literal))
            {
                return false;
            }
            return this.Equals((Literal)o);
        }

        public bool Equals(Literal other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return other.negativeLiteral.Equals(this.negativeLiteral) && Equals(other.AtomicSentence, this.AtomicSentence);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (this.negativeLiteral.GetHashCode() * 397) ^ (this.AtomicSentence != null ? this.AtomicSentence.GetHashCode() : 0);
            }
        }
    }
}
