using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    using System.Collections.ObjectModel;

    public class Function : ITerm 
    {
        private String functionName;
        private IList<ITerm> terms;

        public Function(string functionName, IList<ITerm> terms) 
        {
            this.functionName = functionName;
            this.terms = terms.ToList();
        }

        public IList<ITerm> Terms
        {
            get
            {
                return new ReadOnlyCollection<ITerm>(this.terms);
            }
        }

        public string GetFunctionName() 
        {
            return functionName;
        }

        public string GetSymbolicName() 
        {
            return this.GetFunctionName();
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
            return v.VisitFunction(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new Function(functionName, this.Terms.ToList());
        }

        public override string ToString() 
        {
            var sb = new StringBuilder();
            sb.Append(this.functionName);
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

            return sb.ToString();
        }

        public bool Equals(Function other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.functionName, this.functionName) && Equals(other.terms, this.terms);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.functionName != null ? this.functionName.GetHashCode() : 0) * 397) ^ (this.terms != null ? this.terms.GetHashCode() : 0);
            }
        }
    }
}
