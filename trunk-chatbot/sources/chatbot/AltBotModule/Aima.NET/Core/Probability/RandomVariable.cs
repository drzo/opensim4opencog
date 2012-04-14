using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability
{
    using Aima.Core.Probability.Reasoning;
    using Aima.Core.Util.Datastructure;

    public class RandomVariable 
    {
        private string name;

        private Dictionary<string, double> distribution;

        public IList<string> States { get; private set; }

        public RandomVariable(IList<string> states) : this("HiddenState", states) {
        }

        public RandomVariable(string name, IList<string> states) {
            this.name = name;
            this.States = states;
            this.distribution = new Dictionary<string, double>();
            int numberOfStates = states.Count;
            double initialProbability = 1.0 / numberOfStates;
            foreach (string s in states) {
                distribution[s] = initialProbability;
            }
        }

        private RandomVariable(string name, IList<string> states,
                Dictionary<string, double> distribution) {
            this.name = name;
            this.States = states;
            this.distribution = distribution;
        }

        public void SetProbabilityOf(string state, double probability) {
            if (this.States.Contains(state)) {
                distribution[state] = probability;
            } else {
                throw new ApplicationException(state + "  is an invalid state");
            }
        }

        public double GetProbabilityOf(string state) {
            if (this.States.Contains(state)) {
                return distribution[state];
            } else {
                throw new ApplicationException(state + "  is an invalid state");
            }
        }

        public RandomVariable Duplicate() {
            var probs = new Dictionary<string, double>();
            foreach (string key in distribution.Keys)
            {
                probs[key] = distribution[key];
            }
            return new RandomVariable(name, this.States, probs);

        }

        public void Normalize() {
            IList<double> probs = new List<double>();
            foreach (string s in this.States) {
                probs.Add(distribution[s]);
            }
            IList<double> newProbs = Util.Util.Normalize(probs);
            for (int i = 0; i < this.States.Count; i++) {
                distribution[this.States[i]] = newProbs[i];
            }
        }

        public Matrix AsMatrix() {
            Matrix m = new Matrix(this.States.Count, 1);
            for (int i = 0; i < this.States.Count; i++) {
                m.Set(i, 0, distribution[this.States[i]]);
            }
            return m;

        }

        public void UpdateFrom(Matrix aMatrix) {
            for (int i = 0; i < this.States.Count; i++) {
                distribution[this.States[i]] = aMatrix.Get(i, 0);
            }

        }

        public RandomVariable CreateUnitBelief() {
            RandomVariable result = this.Duplicate();
            foreach (string s in this.States) {
                result.SetProbabilityOf(s, 1.0);
            }
            return result;
        }

        public override string ToString() {
            return this.AsMatrix().ToString();
        }

        public ParticleSet ToParticleSet(HiddenMarkovModel hmm,
                IRandomizer randomizer, int numberOfParticles) {
            ParticleSet result = new ParticleSet(hmm);
            for (int i = 0; i < numberOfParticles; i++) {
                double rvalue = randomizer.NextDouble();
                string state = this.GetStateForRandomNumber(rvalue);
                result.Add(new Particle(state, 0));
            }
            return result;
        }

        private string GetStateForRandomNumber(double rvalue) {
            double total = 0.0;
            foreach (string s in this.States) {
                total = total + distribution[s];
                if (total >= rvalue) {
                    return s;
                }
            }
            throw new ApplicationException("cannot handle " + rvalue);
        }
    }

}
