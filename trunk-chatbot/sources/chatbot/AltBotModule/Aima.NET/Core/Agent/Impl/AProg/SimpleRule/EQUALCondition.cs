using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg.SimpleRule
{
    /// <summary>
    /// Implementation of an EQUALity condition.
    /// </summary>
    public class EQUALCondition : Condition 
    {
        private object key;

        private object value;

        public EQUALCondition(object aKey, object aValue) 
        {
            if (aKey == null) throw new ArgumentNullException();
            if (aValue == null) throw new ArgumentNullException();

            key = aKey;
            value = aValue;
        }

        public override bool Evaluate(ObjectWithDynamicAttributes p) 
        {
            return value.Equals(p.GetAttribute(key));
        }

        public override String ToString() 
        {
            StringBuilder sb = new StringBuilder();

            return String.Format("{0} == {1}", key, value); 
        }
    }
}
