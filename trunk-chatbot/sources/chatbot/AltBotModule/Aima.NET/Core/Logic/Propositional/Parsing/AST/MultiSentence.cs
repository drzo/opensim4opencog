using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    public class MultiSentence : ComplexSentence {
        public string Operator { get; private set; }

        public IList<Sentence> Sentences { get; private set; }

        public MultiSentence(String oper, IList<Sentence> sentences) {
            this.Operator = oper;
            this.Sentences = sentences;
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
            if (o.GetType() != typeof(MultiSentence))
            {
                return false;
            }
            return Equals((MultiSentence)o);
        }

        public override string ToString() {
            String part1 = "( " + Operator + " ";
            for (int i = 0; i < Sentences.Count; i++) {
                part1 = part1 + Sentences[i].ToString() + " ";
            }
            return part1 + " ) ";
        }

        public override object Accept(IPLVisitor plv, ISet<Sentence> arg) {
            return plv.VisitMultiSentence(this, arg);
        }

        public bool Equals(MultiSentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Operator, this.Operator) && Equals(other.Sentences, this.Sentences);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.Operator != null ? this.Operator.GetHashCode() : 0) * 397) ^ (this.Sentences != null ? this.Sentences.GetHashCode() : 0);
            }
        }
    }
}
