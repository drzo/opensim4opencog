using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    using Aima.Core.Util.Datastructure;

    public class MDPTransition<TState, TAction> 
    {
        private Triplet<TState, TAction, TState> triplet;

        public MDPTransition(TState initial, TAction action,
                TState destination) {
            this.triplet = new Triplet<TState, TAction, TState>(
                    initial, action, destination);
        }

        public TState GetInitialState() {
            return triplet.GetFirst();
        }

        public TAction GetAction() {
            return triplet.GetSecond();
        }

        public TState GetDestinationState() {
            return triplet.GetThird();
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
            if (o.GetType() != typeof(MDPTransition<TState, TAction>))
            {
                return false;
            }
            return Equals((MDPTransition<TState, TAction>)o);
        }

        public override string ToString() {
            return triplet.ToString();
        }

        public bool Equals(MDPTransition<TState, TAction> other)
        {
            return Equals(other.triplet, this.triplet);
        }

        public override int GetHashCode()
        {
            return (this.triplet != null ? this.triplet.GetHashCode() : 0);
        }
    }
}
