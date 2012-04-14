using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Parsing.AST
{
    public class Variable : ITerm 
    {
        public string Value { get; private set; }

        public int Indexical
        {
            get
            {
                return this.indexical;
            }
            set
            {
                this.indexical = value;
            }
        }

        private int indexical = -1;

        public Variable(string s) 
        {
            this.Value = s.Trim();
        }

        public Variable(string s, int idx) 
        {
            this.Value = s.Trim();
            this.Indexical = idx;
        }

        public String GetSymbolicName() 
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
            return v.VisitVariable(this, arg);
        }

        public IFOLNode Copy() 
        {
            return new Variable(this.Value, this.Indexical);
        }

        public string GetIndexedValue() 
        {
            return this.Value + this.Indexical;
        }

        public override bool Equals(object o) 
        {
            return Equals(o as Variable);
        }

        public override string ToString() 
        {
            return this.Value;
        }

        public bool Equals(Variable other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return other.indexical == this.indexical && Equals(other.Value, this.Value);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (this.indexical * 397) ^ (this.Value != null ? this.Value.GetHashCode() : 0);
            }
        }
    }
}
