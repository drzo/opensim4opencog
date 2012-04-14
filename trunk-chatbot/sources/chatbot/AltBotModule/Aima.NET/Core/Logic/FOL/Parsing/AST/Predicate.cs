using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    public class Predicate : IAtomicSentence 
    {
        public string PredicateName { get; private set; }

        public IList<ITerm> Terms { get; private set; }

        private string stringRep;
        
        public Predicate(string predicateName, IList<ITerm> terms) 
        {
            this.PredicateName = predicateName;
            this.Terms = terms;
        }

        public string GetSymbolicName() 
        {
            return this.PredicateName;
        }

        public bool IsCompound() 
        {
            return true;
        }

        public IList<IFOLNode> GetArgs()
        {
            return this.Terms.Cast<IFOLNode>().ToList();
        }

        public object Accept(IFOLVisitor v, object arg) 
        {
            return v.VisitPredicate(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new Predicate(this.PredicateName, this.Terms.ToList());
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
            if (o.GetType() != typeof(Predicate))
            {
                return false;
            }
            return Equals((Predicate)o);
        }

        public override String ToString() 
        {
            if (null == stringRep) {
                StringBuilder sb = new StringBuilder();
                sb.Append(this.PredicateName);
                sb.Append("(");

                var first = true;
                foreach (var t in this.Terms) 
                {
                    if (first) 
                    {
                        first = false;
                    } 
                    else 
                    {
                        sb.Append(",");
                    }
                    sb.Append(t.ToString());
                }

                sb.Append(")");
                this.stringRep = sb.ToString();
            }

            return this.stringRep;
        }

        public bool Equals(Predicate other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.PredicateName, this.PredicateName) && Equals(other.Terms, this.Terms);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.PredicateName != null ? this.PredicateName.GetHashCode() : 0) * 397) ^ (this.Terms != null ? this.Terms.GetHashCode() : 0);
            }
        }
    }
}
