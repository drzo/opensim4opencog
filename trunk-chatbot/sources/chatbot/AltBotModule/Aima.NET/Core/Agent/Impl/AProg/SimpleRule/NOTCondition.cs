using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg.SimpleRule
{
    /// <summary>
    /// Implementation of a NOT condition.
    /// </summary>
    public class NOTCondition : Condition 
    {
        private Condition con;

        public NOTCondition(Condition aCon) 
        {
            if (aCon == null) throw new ArgumentNullException();

            con = aCon;
        }

        public override bool Evaluate(ObjectWithDynamicAttributes p) 
        {
            return (!con.Evaluate(p));
        }

        public override string ToString() {
            return String.Format("![{0}]",con);
        }
    }
}
