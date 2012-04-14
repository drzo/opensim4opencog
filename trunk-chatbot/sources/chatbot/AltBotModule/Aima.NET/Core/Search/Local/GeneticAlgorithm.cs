using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Search.Local
{
    using System.Diagnostics;

    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.8, page 129.
    /// 
    /// <code><![CDATA[
    /// function GENETIC-ALGORITHM(population, FITNESS-FN) returns an individual
    ///   inputs: population, a Set of individuals
    ///           FITNESS-FN, a function that measures the fitness of an individual
    ///           
    ///   repeat
    ///     new_population <- empty Set
    ///     for i = 1 to SIZE(population) do
    ///       x <- RANDOM-SELECTION(population, FITNESS-FN)
    ///       y <- RANDOM-SELECTION(population, FITNESS-FN)
    ///       child <- REPRODUCE(x, y)
    ///       if (small random probability) then child <- MUTATE(child)
    ///       add child to new_population
    ///     population <- new_population
    ///   until some individual is fit enough, or enough time has elapsed
    ///   return the best individual in population, according to FITNESS-FN
    /// --------------------------------------------------------------------------------
    /// function REPRODUCE(x, y) returns an individual
    ///   inputs: x, y, parent individuals
    ///   
    ///   n <- LENGTH(x); c <- random number from 1 to n
    ///   return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n))
    /// ]]></code>
    /// 
    /// Figure 4.8 A genetic algorithm. The algorithm is the same as the one diagrammed
    /// in Figure 4.6, with one variation: in this more popular version, each mating of 
    /// two parents produces only one offspring, not two.
    /// </summary>
    public class GeneticAlgorithm 
    {
        //
        protected Metrics Metrics = new Metrics();
        protected static readonly string PopulationSize = "populationSize";
        protected static readonly string Iterations = "iterations";

        //
        private readonly int individualLength;
        private readonly char[] finiteAlphabet;
        private readonly double mutationProbability;
        private readonly Random random = new Random();

        public GeneticAlgorithm(int individualLength,
                ISet<char> finiteAlphabet, double mutationProbability) {
            this.individualLength = individualLength;
            this.finiteAlphabet = finiteAlphabet.ToArray();
            this.mutationProbability = mutationProbability;
            Debug.Assert (this.mutationProbability >= 0.0 && this.mutationProbability <= 1.0);
        }

        // function GENETIC-ALGORITHM(population, FITNESS-FN) returns an individual
        // inputs: population, a Set of individuals
        // FITNESS-FN, a function that measures the fitness of an individual
        public string GetBestIndividual(ISet<string> population, IFitnessFunction fitnessFn, IGoalTest goalTest) 
        {
            string bestIndividual = null;

            this.ValidatePopulation(population);
            this.ClearInstrumentation();
            this.SetPopulationSize(population.Count);

            // repeat
            int cnt = 0;
            do {
                bestIndividual = this.Ga(population, fitnessFn);
                cnt++;
                // until some individual is fit enough, or enough time has elapsed
            } while (!goalTest.IsGoalState(bestIndividual));
            this.SetIterations(cnt);

            // return the best individual in population, according to FITNESS-FN
            return bestIndividual;
        }

        // function GENETIC-ALGORITHM(population, FITNESS-FN) returns an individual
        // inputs: population, a Set of individuals
        // FITNESS-FN, a function that measures the fitness of an individual
        public string GetBestIndividual(ISet<string> population,
                IFitnessFunction fitnessFn, int iterations) {
            string bestIndividual = null;

            this.ValidatePopulation(population);
            this.ClearInstrumentation();
            this.SetPopulationSize(population.Count);

            // repeat
            // until some individual is fit enough, or enough time has elapsed
            for (int i = 0; i < iterations; i++) {
                bestIndividual = this.Ga(population, fitnessFn);
            }
            this.SetIterations(iterations);

            // return the best individual in population, according to FITNESS-FN
            return bestIndividual;
        }

        public void ClearInstrumentation() {
            this.SetPopulationSize(0);
            this.SetIterations(0);
        }

        public Metrics GetMetrics() {
            return this.Metrics;
        }

        public int GetPopulationSize() {
            return this.Metrics.GetInt(PopulationSize);
        }

        public void SetPopulationSize(int size) {
            this.Metrics.Set(PopulationSize, size);
        }

        public int GetIterations() {
            return this.Metrics.GetInt(Iterations);
        }

        public void SetIterations(int cnt) {
            this.Metrics.Set(Iterations, cnt);
        }

        private void ValidatePopulation(ISet<string> population) {
            // Require at least 1 individual in population in order
            // for algorithm to work
            if (population.Count < 1) {
                throw new ArgumentOutOfRangeException("population", "Must start with at least a population of size 1");
            }
            // string lengths are assumed to be of fixed size,
            // therefore ensure initial populations lengths correspond to this
            foreach (string individual in population) {
                if (individual.Length != this.individualLength) 
                {
                    throw new InvalidOperationException("Individual [" + individual
                            + "] in population is not the required Length of "
                            + this.individualLength);
                }
            }
        }

        private string Ga(ISet<string> population, IFitnessFunction fitnessFn) {
            // new_population <- empty Set
            ISet<string> newPopulation = new HashedSet<string>();

            // for i = 1 to SIZE(population) do
            for (int i = 0; i < population.Count; i++) {
                // x <- RANDOM-SELECTION(population, FITNESS-FN)
                string x = this.RandomSelection(population, fitnessFn);
                // y <- RANDOM-SELECTION(population, FITNESS-FN)
                string y = this.RandomSelection(population, fitnessFn);
                // child <- REPRODUCE(x, y)
                string child = this.Reproduce(x, y);
                // if (small random probability) then child <- MUTATE(child)
                if (random.NextDouble() <= this.mutationProbability) {
                    child = this.Mutate(child);
                }
                // add child to new_population
                newPopulation.Add(child);
            }
            // population <- new_population
            population.Clear();
            population.UnionWith(newPopulation);

            return this.RetrieveBestIndividual(population, fitnessFn);
        }

        private string RandomSelection(ISet<string> population,
                IFitnessFunction fitnessFn) {
            string selected = null;

            // Determine all of the fitness values
            double[] fValues = new double[population.Count];
            string[] popArray = population.ToArray();
            for (int i = 0; i < popArray.Length; i++) {
                fValues[i] = fitnessFn.GetValue(popArray[i]);
            }

            // Normalize the fitness values
            fValues = Util.Util.Normalize(fValues);
            double prob = random.NextDouble();
            double totalSoFar = 0.0;
            for (int i = 0; i < fValues.Length; i++) {
                // Are at last element so assign by default
                // in case there are rounding issues with the normalized values
                totalSoFar += fValues[i];
                if (prob <= totalSoFar) {
                    selected = popArray[i];
                    break;
                }
            }

            // selected may not have been assigned
            // if there was a rounding error in the
            // addition of the normalized values (i.e. did not total to 1.0)
            if (null == selected) {
                // Assign the last value
                selected = popArray[popArray.Length - 1];
            }

            return selected;
        }

        // function REPRODUCE(x, y) returns an individual
        // inputs: x, y, parent individuals
        private string Reproduce(string x, string y) {
            // n <- LENGTH(x);
            // Note: this is = this.individualLength
            // c <- random number from 1 to n
            int c = this.RandomOffset(individualLength);
            // return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n))
            return x.Substring(0, c) + y.Substring(c);
        }

        private string Mutate(string individual) {
            StringBuilder mutInd = new StringBuilder(individual);

            int posOffset = this.RandomOffset(individualLength);
            int charOffset = this.RandomOffset(finiteAlphabet.Length);

            mutInd[posOffset] = finiteAlphabet[charOffset];

            return mutInd.ToString();
        }

        private string RetrieveBestIndividual(ISet<string> population, IFitnessFunction fitnessFn) 
        {
            string bestIndividual = null;
            double bestSoFarFValue = double.NegativeInfinity;

            foreach (string individual in population) 
            {
                var fitnessValue = fitnessFn.GetValue(individual);
                if (fitnessValue > bestSoFarFValue) 
                {
                    bestIndividual = individual;
                    bestSoFarFValue = fitnessValue;
                }
            }

            return bestIndividual;
        }

        private int RandomOffset(int Length) 
        {
            return random.Next(Length);
        }
    }
}
