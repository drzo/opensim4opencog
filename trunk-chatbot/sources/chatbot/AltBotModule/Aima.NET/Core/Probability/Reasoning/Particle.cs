using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    public class Particle
    {

        public string State { get; private set; }

        public double Weight { get; set; }

        public Particle(string state, double weight)
        {
            this.State = state;
            this.Weight = weight;
        }

        public Particle(string state)
            : this(state, 0)
        {
        }

        public bool HasState(string aState)
        {
            return this.State.Equals(aState);
        }

    }
}
