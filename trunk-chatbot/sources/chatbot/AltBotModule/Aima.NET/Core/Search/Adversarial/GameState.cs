using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Adversarial
{
    public class GameState 
    {
        private Dictionary<string, object> state;

        public GameState() 
        {
            state = new Dictionary<string, object>();
        }

        public override bool Equals(object anotherState)
        {
            if (ReferenceEquals(null, anotherState) || !(anotherState is GameState))
            {
                return false;
            }
            return Equals((GameState)anotherState);
        }

        public object this[string key]
        {
            get
            {
                return this.state[key];
            }
            
            set
            {
                this.state[key] = value;
            }
        }

        public bool Equals(GameState other)
        {
            if (ReferenceEquals(null, other))
            {
                return false;
            }
            if (ReferenceEquals(this, other))
            {
                return true;
            }
            return Equals(other.state, this.state);
        }

        public override int GetHashCode()
        {
            return (this.state != null ? this.state.GetHashCode() : 0);
        }
    }
}
