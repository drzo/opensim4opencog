using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg.SimpleRule
{
    /// <summary>
    /// Implementation of an AND condition.
    /// </summary>
    public class ANDCondition : Condition 
    {
        private Condition left;

        private Condition right;

        public ANDCondition(Condition aLeftCon, Condition aRightCon) 
        {
            if (aLeftCon == null)
            {
                throw new ArgumentNullException();
            }

            if (aRightCon == null)
            {
                throw new ArgumentNullException();
            }

            this.left = aLeftCon;
            this.right = aRightCon;
        }

        public override bool Evaluate(ObjectWithDynamicAttributes p) 
        {
            return (left.Evaluate(p) && right.Evaluate(p));
        }

        public override string ToString() 
        {
            return String.Format("[{0} && {1}]",left, right);
        }
    }
}
