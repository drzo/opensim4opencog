using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Datastructure
{
public class Triplet<TX, TY, TZ> 
{
    // TODO: just make sure that final keyword is really replaceable with readonly
    private readonly TX x;

    private readonly TY y;

    private readonly TZ z;

    public Triplet(TX x, TY y, TZ z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public TX GetFirst() {
        return x;
    }

    public TY GetSecond() {
        return y;
    }

    public TZ GetThird() {
        return z;
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
        if (o.GetType() != typeof(Triplet<TX, TY, TZ>))
        {
            return false;
        }
        return Equals((Triplet<TX, TY, TZ>)o);
    }

    public override string ToString() 
    {
        return String.Format("< {0} , {1} , {2} >", this.x, this.y, this.z);
    }

    public bool Equals(Triplet<TX, TY, TZ> other)
    {
        if (ReferenceEquals(null, other))
        {
            return false;
        }
        if (ReferenceEquals(this, other))
        {
            return true;
        }
        return Equals(other.x, this.x) && Equals(other.y, this.y) && Equals(other.z, this.z);
    }

    public override int GetHashCode()
    {
        unchecked
        {
            var result = this.x.GetHashCode();
            result = (result * 397) ^ this.y.GetHashCode();
            result = (result * 397) ^ this.z.GetHashCode();
            return result;
        }
    }
}

}
