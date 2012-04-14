using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg.SimpleRule
{
    /// <summary>
    /// Base abstract class for describing conditions.
    /// </summary>
    public abstract class Condition 
    {
        public abstract bool Evaluate(ObjectWithDynamicAttributes p);

        public override bool Equals(object o) 
        {
            if (!(o is Condition)) 
            {
                return base.Equals(o);
            }
            return (ToString().Equals(o.ToString()));
        }

        public override int GetHashCode() 
        {
            return ToString().GetHashCode();
        }
    }
}
