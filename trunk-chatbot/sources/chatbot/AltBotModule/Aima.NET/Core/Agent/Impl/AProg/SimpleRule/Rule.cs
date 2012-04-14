using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl.AProg.SimpleRule
{
    //TODO: can this be replaced by linq validation rules?
    /// <summary>
    /// A simple implementation of a "condition-action rule".
    /// </summary>
    public class Rule 
    {
        private Condition con;

        private IAction action;

        public Rule(Condition aCon, IAction anAction) 
        {
            if (aCon == null) throw new ArgumentNullException("aCon");
            if (anAction == null) throw new ArgumentNullException("anAction");

            con = aCon;
            action = anAction;
        }

        public bool Evaluate(ObjectWithDynamicAttributes p) {
            return (con.Evaluate(p));
        }

        public IAction GetAction() {
            return action;
        }

        public override bool Equals(object o) {
            if (o == null || !(o is Rule)) {
                return base.Equals(o);
            }
            return (ToString().Equals(o.ToString()));
        }

        
        public override int GetHashCode() {
            return ToString().GetHashCode();
        }

        
        public override string ToString() {
            StringBuilder sb = new StringBuilder();

            return sb.Append("if ").Append(con).Append(" then ").Append(action)
                    .Append(".").ToString();
        }
    }
}
