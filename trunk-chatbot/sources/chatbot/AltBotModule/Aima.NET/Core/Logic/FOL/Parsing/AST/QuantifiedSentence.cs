using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using System.Collections.ObjectModel;

    public class QuantifiedSentence : ISentence 
    {
        public string Quantifier { get; private set; }

        public IList<Variable> Variables
        {
            get
            {
                return new ReadOnlyCollection<Variable>(this.variables);
            }
        }

        private IList<Variable> variables;

        public ISentence Quantified { get; private set; }

        private IList<IFOLNode> args;

        public QuantifiedSentence(string quantifier, IList<Variable> variables, ISentence quantified) 
        {
            this.Quantifier = quantifier;
            this.variables = variables.ToList();
            this.Quantified = quantified;
            this.args = variables.Cast<IFOLNode>().ToList();
            this.args.Add(quantified);
        }

        public string GetSymbolicName()
        {
            return this.Quantifier;
        }

        public bool IsCompound() 
        {
            return true;
        }

        public IList<IFOLNode> GetArgs() 
        {
            return new ReadOnlyCollection<IFOLNode>(args);
        }

        public object Accept(IFOLVisitor v, object arg) 
        {
            return v.VisitQuantifiedSentence(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new QuantifiedSentence(this.Quantifier, this.Variables.ToList(), (ISentence) this.Quantified.Copy());
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
            if (o.GetType() != typeof(QuantifiedSentence))
            {
                return false;
            }
            return Equals((QuantifiedSentence)o);
        }

        public override string ToString() 
        {
            var sb = new StringBuilder();
            sb.Append(this.Quantifier);
            sb.Append(" ");
            foreach (var v in this.Variables) 
            {
                sb.Append(v.ToString());
                sb.Append(" ");
            }
            sb.Append(this.Quantified);

            return sb.ToString();
        }

        public bool Equals(QuantifiedSentence other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.variables, this.variables) && Equals(other.Quantifier, this.Quantifier) && Equals(other.Quantified, this.Quantified);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                int result = (this.variables != null ? this.variables.GetHashCode() : 0);
                result = (result * 397) ^ (this.Quantifier != null ? this.Quantifier.GetHashCode() : 0);
                result = (result * 397) ^ (this.Quantified != null ? this.Quantified.GetHashCode() : 0);
                return result;
            }
        }
    }
}
