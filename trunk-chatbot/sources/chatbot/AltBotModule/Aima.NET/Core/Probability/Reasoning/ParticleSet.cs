using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    public class ParticleSet
    {

        private IList<Particle> particles;

        private HiddenMarkovModel hmm;

        public ParticleSet(HiddenMarkovModel hmm)
        {
            particles = new List<Particle>();
            this.hmm = hmm;
        }

        // Use these two to get the filtered set directly. This is the only method a
        // user class needs to call.
        // The other methods are public only to be accessible to tests.

        public ParticleSet Filter(string perception, IRandomizer r)
        {
            return this.GenerateParticleSetForPredictedState(r).PerceptionUpdate(
                    perception, r);
        }

        public ParticleSet Filter(string action, string perception, IRandomizer r)
        {
            return this.GenerateParticleSetForPredictedState(action, r)
                    .PerceptionUpdate(perception, r);
        }

        // these are internal methods. public only to facilitate testing
        public int NumberOfParticlesWithState(string state) {
        int total = 0;
        foreach (Particle p in particles) {
            if (p.HasState(state)) {
                total += 1;
            }
        }
        return total;
    }

        public void Add(Particle particle)
        {
            particles.Add(particle);

        }

        public int Count
        {
            get
            {
                return particles.Count;
            }
            
        }

        public RandomVariable ToRandomVariable() 
        {
            IList<string> states = new List<string>();
            Dictionary<string, int> stateCount = new Dictionary<string, int>();
            foreach (Particle p in particles) 
            {
                string state = p.State;
                if (!(states.Contains(state))) 
                {
                    states.Add(state);
                    stateCount[state] = 0;
                }

                stateCount[state] = stateCount[state] + 1;

            }

            RandomVariable result = new RandomVariable(states);
            foreach (string state in stateCount.Keys) {
                result.SetProbabilityOf(state,
                        ((double) stateCount[state] / particles.Count));
            }
            return result;
        }

        public ParticleSet GenerateParticleSetForPredictedState(
                IRandomizer randomizer)
        {
            return this.GenerateParticleSetForPredictedState(HmmConstants.DoNothing,
                    randomizer);
        }

        public ParticleSet GenerateParticleSetForPredictedState(string action,
                IRandomizer randomizer) {
            ParticleSet predictedParticleSet = new ParticleSet(this.hmm);
            foreach (Particle p in particles) {
                string newState = hmm.TransitionModel.GetStateForProbability(
                        p.State, action, randomizer.NextDouble());

                Particle generatedParticle = new Particle(newState);
                predictedParticleSet.Add(generatedParticle);
            }
            return predictedParticleSet;
        }

        public ParticleSet PerceptionUpdate(string perception, IRandomizer r) 
        {
            // compute Particle Weight
            foreach (Particle p in particles) {
                double particleWeight = hmm.SensorModel.Get(p.State, perception);
                p.Weight = particleWeight;
            }

            // weighted sample to create new ParticleSet
            ParticleSet result = new ParticleSet(hmm);
            while (result.Count != Count) {
                foreach (Particle p in particles) {
                    double probability = r.NextDouble();
                    if (probability <= p.Weight) {
                        if (result.Count < Count) {
                            result.Add(new Particle(p.State, p.Weight));
                        }
                    }
                }

            }
            return result;
        }

        public Particle GetParticle(int i)
        {
            return particles[i];
        }
    }

}
