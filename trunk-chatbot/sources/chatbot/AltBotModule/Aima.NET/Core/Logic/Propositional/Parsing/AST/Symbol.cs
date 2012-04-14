using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Parsing.AST
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public class Symbol : AtomicSentence 
    {
        public string Value { get; private set; }

        public Symbol(string value) {
            this.Value = value;
        }

        public override bool Equals(object o) 
        {
            if (!(o is Symbol))
            {
                return false;
            }
            return Equals((Symbol) o);

        }

        public string GetValue() {
            return this.Value;
        }

        public override string ToString() {
            return this.GetValue();
        }

        public override object Accept(IPLVisitor plv, ISet<Sentence> arg)
        {
            return plv.VisitSymbol(this, arg);
        }

        public bool Equals(Symbol other)
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
