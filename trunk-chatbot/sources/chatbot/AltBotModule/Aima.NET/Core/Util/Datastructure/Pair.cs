using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
    public class Pair<TX, TY> {
        private readonly TX a;

        private readonly TY b;

        public Pair(TX a, TY b) {
            this.a = a;
            this.b = b;
        }

        public TX GetFirst() {
            return a;
        }

        public TY GetSecond() {
            return b;
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
            if (o.GetType() != typeof(Pair<TX, TY>))
            {
                return false;
            }
            return Equals((Pair<TX, TY>)o);
        }

        public String toString() {
            return String.Format("< {0} , {1} > ", this.GetFirst(), this.GetSecond());
        }

        public bool Equals(Pair<TX, TY> other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.a, this.a) && Equals(other.b, this.b);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (this.a.GetHashCode() * 397) ^ this.b.GetHashCode();
            }
        }
    }

}
