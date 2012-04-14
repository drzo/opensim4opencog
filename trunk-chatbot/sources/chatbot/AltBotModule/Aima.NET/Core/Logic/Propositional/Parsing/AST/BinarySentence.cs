using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    public class BinarySentence : ComplexSentence 
    {
        public string Operator { get; private set; }

        public Sentence First { get; private set; }

        public Sentence Second { get; private set; }

        public BinarySentence(string oper, Sentence first, Sentence second) {
            this.Operator = oper;
            this.First = first;
            this.Second = second;

        }

        public override bool Equals(object o) 
        {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            if (o.GetType() != typeof(BinarySentence))
            {
                return false;
            }
            return Equals((BinarySentence)o);
        }

        public override string ToString() {
            return " ( " + First.ToString() + " " + Operator + " " + Second.ToString() + " )";
        }

        public override object Accept(IPLVisitor plv, ISet<Sentence> arg) 
        {
            return plv.VisitBinarySentence(this, arg);
        }

        public bool IsOrSentence() {
            return (Operator.Equals("OR"));
        }

        public bool IsAndSentence() {
            return (Operator.Equals("AND"));
        }

        public bool IsImplication() {
            return (Operator.Equals("=>") || Operator.Equals("IMPLIES"));
        }

        public bool IsBiconditional() {
            return (Operator.Equals("<=>") || Operator.Equals("EQUIV"));
        }

        public bool FirstTermIsAndSentence() {
            return (First is BinarySentence) && (((BinarySentence) First).IsAndSentence());
        }

        public bool SecondTermIsAndSentence() {
            return (Second is BinarySentence) && (((BinarySentence) Second).IsAndSentence());
        }

        public bool Equals(BinarySentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Operator, this.Operator) && Equals(other.First, this.First) && Equals(other.Second, this.Second);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                int result = (this.Operator != null ? this.Operator.GetHashCode() : 0);
                result = (result * 397) ^ (this.First != null ? this.First.GetHashCode() : 0);
                result = (result * 397) ^ (this.Second != null ? this.Second.GetHashCode() : 0);
                return result;
            }
        }
    }
}
