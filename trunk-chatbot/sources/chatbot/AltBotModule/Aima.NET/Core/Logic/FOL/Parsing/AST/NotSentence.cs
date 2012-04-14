using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using System.Collections.ObjectModel;

    public class NotSentence : ISentence 
    {
        public ISentence Negated { get; private set; }
        private IList<ISentence> args = new List<ISentence>();

        public NotSentence(ISentence negated) 
        {
            this.Negated = negated;
            args.Add(negated);
        }

        public string GetSymbolicName()
        {
            return Connectors.Not;
        }

        public bool IsCompound() 
        {
            return true;
        }

        public IList<IFOLNode> GetArgs()
        {
            return new ReadOnlyCollection<IFOLNode>(args.Cast<IFOLNode>().ToList());
        }

        public object Accept(IFOLVisitor v, object arg) 
        {
            return v.VisitNotSentence(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new NotSentence((ISentence) this.Negated.Copy());
        }

        public override string ToString() 
        {
            var sb = new StringBuilder();
            sb.Append("NOT(");
            sb.Append(this.Negated);
            sb.Append(")");

            return sb.ToString();
        }

        public bool Equals(NotSentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.args, this.args) && Equals(other.Negated, this.Negated);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.args != null ? this.args.GetHashCode() : 0) * 397) ^ (this.Negated != null ? this.Negated.GetHashCode() : 0);
            }
        }
    }

}
