using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    public class UnarySentence : ComplexSentence 
    {
        public Sentence Negated { get; private set; }

        public UnarySentence(Sentence negated) {
            this.Negated = negated;
        }

        public override bool Equals(object o) 
        {
            if (!(o is UnarySentence))
            {
                return false;
            }
            return Equals((UnarySentence)o);

        }

        public override string ToString() {
            return " ( NOT " + this.Negated + " ) ";
        }

        public override object Accept(IPLVisitor plv, ISet<Sentence> arg)
        {
            return plv.VisitNotSentence(this, arg);
        }

        public bool Equals(UnarySentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }

            return Equals(other.Negated, this.Negated);
        }

        public override int GetHashCode()
        {
            return this.Negated != null ? this.Negated.GetHashCode() : 0;
        }
    }
}
