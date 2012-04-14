using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Online
{
    using Aima.Core.Agent;

    public class StateAction
    {
        private readonly object state;
        private readonly IAction action;

        public StateAction(object state, IAction action) 
        {
            this.state = state;
            this.action = action;
        }

        public object State
        {
            get
            {
                return this.state;
            }
        }

        public IAction Action
        {
            get
            {
                return this.action;
            }
        }

        public override bool Equals(object o) 
        {
            if (ReferenceEquals(null, o))
            {
                return false;
            }
            if (ReferenceEquals(this, o))
            {
                return true;
            }
            if (o.GetType() != typeof(StateAction))
            {
                return false;
            }
            return Equals((StateAction)o);
        }

        public override string ToString() 
        {
            return "(" + this.State + ", " + this.Action + ")";
        }

        public bool Equals(StateAction other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.state, this.state) && Equals(other.action, this.action);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((this.state != null ? this.state.GetHashCode() : 0) * 397) ^ (this.action != null ? this.action.GetHashCode() : 0);
            }
        }
    }

}
