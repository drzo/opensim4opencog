using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using System.Collections.ObjectModel;

    public class TermEquality : IAtomicSentence 
    {
        public ITerm Term1 { get; private set; }

        public ITerm Term2 { get; private set; }

        private IList<ITerm> terms = new List<ITerm>();

        public static string GetEqualitySynbol() 
        {
            return "=";
        }

        public TermEquality(ITerm term1, ITerm term2) 
        {
            this.Term1 = term1;
            this.Term2 = term2;
            terms.Add(term1);
            terms.Add(term2);
        }


        public string GetSymbolicName() 
        {
            return GetEqualitySynbol();
        }

        public bool IsCompound() 
        {
            return true;
        }

        public IList<IFOLNode> GetArgs()
        {
            return new ReadOnlyCollection<IFOLNode>(terms.Cast<IFOLNode>().ToList());
        }

        public object Accept(IFOLVisitor v, object arg) 
        {
            return v.VisitTermEquality(this, arg);
        }

        public IFOLNode Copy() 
        {
            //can safely cast to ITerm as it's just a tagging interface 
            return new TermEquality((ITerm) this.Term1.Copy(), (ITerm) this.Term2.Copy());
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
            if (o.GetType() != typeof(TermEquality))
            {
                return false;
            }
            return Equals((TermEquality)o);
        }

        public override string ToString() 
        {
            return String.Format("{0} = {1}", this.Term1, this.Term2);
        }

        public bool Equals(TermEquality other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Term1, this.Term1) && Equals(other.Term2, this.Term2);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.Term1 != null ? this.Term1.GetHashCode() : 0) * 397) ^ (this.Term2 != null ? this.Term2.GetHashCode() : 0);
            }
        }
    }
}
