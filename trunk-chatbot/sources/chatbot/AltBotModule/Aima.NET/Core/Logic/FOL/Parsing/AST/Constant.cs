using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    public class Constant : ITerm 
    {
        public string Value { get; private set; }

        public Constant(String s) {
            this.Value = s;
        }

        public string GetSymbolicName() 
        {
            return this.Value;
        }

        public bool IsCompound() 
        {
            return false;
        }

        public IList<IFOLNode> GetArgs() 
        {
            // Is not Compound, therefore should
            // return null for its arguments
            return null;
        }

        public object Accept(IFOLVisitor v, object arg)
        {
            return v.VisitConstant(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new Constant(this.Value);
        }

        public override string ToString() 
        {
            return this.Value;
        }

        public bool Equals(Constant other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.Value, this.Value);
        }

        public override int GetHashCode()
        {
            return (this.Value != null ? this.Value.GetHashCode() : 0);
        }
    }

}
