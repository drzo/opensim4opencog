using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LogicalParticleFilter1
{
    // Should I include the particle evaluation delegate with each observation
    // or should it be passed in by the PF algorithm
    // and what should I do if I have both ?
    public class Action
    {
        // The action description
        // A probability over a set of Davisonian slots ???
        public Dictionary<string, double> actParms = new Dictionary<string, double>();
    }

    public class Observation
    {
        // An observation of a sense-event and a probability/strength for each sense item
       public Dictionary<string, double> observable = new Dictionary<string, double>();

        // What is the likelihood that particl P generated this observation ??
       public double getLikelihood(Particle p)
       {
           double likelihood = 1e-20;
           //TODO return P(S|O)
           return likelihood;
       }
       public double getLikelihood(Particle p, ParticleEvaluationDelegate PED)
       {
           double likelihood = 1e-20;
           //TODO return P(S|O)
           if (PED != null)
           {
               likelihood = PED(p); 
           }
           return likelihood;
       }
    }

    public class Particle
    {
        // Contains a description of the relevant variables of the state being monitored
        // and the certainty
        public Dictionary<string, double> variables = new Dictionary<string, double>();
        public double prob;
        public double weight;
        public override string ToString()
        {
            string result = "";
            //foreach (string k in variables.Keys )
            //{
            //    result += String .Format (" {0}={1},\n", k, variables[k]);
            //}
            foreach (KeyValuePair<string, double> item in variables.OrderByDescending(key => key.Value))
            {
                result += String.Format(" {0}={1},\n", item.Key, item.Value  );
            }
            result += String.Format(" prob={0},\n", prob );
            result += String.Format(" weight={0}", weight);
            return result;
        }
        public Particle() { }
        public Particle(Particle p)
        {
            this.prob = p.prob;
            this.weight = p.weight;
            foreach (string key in p.variables.Keys)
            {
                this.variables.Add(key, p.variables[key]);
            }
        }

        public void normalize(List <string> constraintSet)
        {
            // Normalizes the internal slots probabilities
            //return;
            foreach (string constraint in constraintSet)
            {
                string[] clist = constraint.Split('|');
                double psum = 0;
                for (int i = 0; i < clist.Length; i++)
                {
                    string key = clist[i];
                    if (!variables.ContainsKey(key))
                    {
                        clist[i] = null;
                        continue;
                    }
                    psum += variables[key];
                }
                if (psum <= 0) continue;
                foreach (string key in clist)
                {
                    if (key == null) continue;
                    variables[key] = variables[key] / psum;
                }
               
            }

        }
        public void normalize0()
        {
            double psum = 0;
            string[] vkeys = variables.Keys.ToArray();
            foreach (string key in vkeys)
            {
                psum += variables[key];
            }
            if (psum <= 0) return;
            foreach (string key in vkeys)
            {
                variables[key] = variables[key] / psum;
            }
        }
        public string ToString(Dictionary<string, string> map)
        {
            // Accepts a dictionary based map of (variablename, format-string)
            string result = "";
            foreach (string k in map.Keys)
            {
                
                result += String.Format(map[k], variables[k]);
            }
            if (map.ContainsKey("prob")) result += String.Format(map["prob"], prob);
            if (map.ContainsKey("weight")) result += String.Format(map["weight"], weight);
            return result;

        }

        public string asDataMt(double threshold)
        {
            string result = "";
            foreach (string k in variables.Keys)
            {
                if (variables[k] >= threshold)
                {
                    result += String.Format("{0}.\n",k);
                }
            }
            foreach (string k in variables.Keys)
            {

                    result += String.Format("prob({0},{1}).\n", k,variables [k]);

            }
            return result;
        }
    }

    public delegate Particle ParticleTransformDelegate(Particle p);
    public delegate double ParticleEvaluationDelegate(Particle p);

    public class Estimator
    {
        // An estimator returns a probability after measuring properties of a particle

        public string rule;
        public string state;
        public string aux;
        public string key;
        public Dictionary<string, double> estimate = new Dictionary<string, double>();

    }
    

    public class SymbolicParticleFilter
    {

        // A particle filter

        public int numberOfParticles = 100;
        public Random rgen = new Random();
        public Particle[] particles;
        public double[] accum;
        public Particle prototype = new Particle();
        public Particle sumParticle = new Particle();
        public Particle meanParticle = new Particle();
        public bool initialized = false;
        public Action inputAct = new Action();
        public Observation inputObservation = new Observation();
        public double totalLikelihood = 0;
        public double totalWeight = 0;
        public double minimumP = 1e-80;
        public Particle[] traceSet;
        public int traceDepth = 64;
        public int traceStack =0;

        public ParticleTransformDelegate processParticleTransform;

        public Dictionary<string, Estimator> state_sense_prob = new Dictionary<string, Estimator>();
        public Dictionary<string, Estimator> state_act_tansition = new Dictionary<string, Estimator>();

        public List<string> constraintSet = new List<string>();
        public List<string> stateList = new List<string>();
        public List<string> actList = new List<string>();
        public List<string> senseList = new List<string>();

        public SymbolicParticleFilter()
        {
            processParticleTransform = defaultTransformParticle;
            particles = new Particle[numberOfParticles];
            accum = new double[numberOfParticles];
            traceSet = new Particle[traceDepth+1];

        }
        public void defMeanParticle()
        {
            foreach (string k in prototype.variables.Keys )
            {
                meanParticle .variables [k]=0;
            }
            for(int i=0;i<numberOfParticles ;i++)
            {
                foreach (string k in prototype.variables.Keys )
                {
                    var particlesvariables = particles[i].variables;
                    if (!particlesvariables.ContainsKey(k)) continue;
                    meanParticle.variables[k] = meanParticle.variables[k] + particlesvariables[k];
                }
            }
            foreach (string k in prototype.variables.Keys )
            {
               meanParticle.variables [k]= meanParticle.variables [k]/numberOfParticles ;
            }

        }
        // Should do XML versions of these

        public void addStateSenseProb(string rule)
        {
            // "state|sense=prob"
            string [] parms = rule.Split('=');
            string key = parms[0];
            string strProb = parms[1];
            string [] keySplit = key.Split('|');
            string state = keySplit [0];
            string aux = keySplit[1];
            if (!state_sense_prob.ContainsKey(key))
            {
                state_sense_prob.Add(key,new Estimator());
            }
            Estimator myEstimate = state_sense_prob[key];
            myEstimate.rule = rule;
            myEstimate.state=state;
            myEstimate.aux = aux;
            if (myEstimate.estimate.ContainsKey(state))
            {
                myEstimate.estimate[state] = double.Parse(strProb);
            }
            else
            {
                myEstimate.estimate.Add(state, double.Parse(strProb));
            }
            if (!stateList.Contains(state)) stateList.Add(state);
            if (!senseList.Contains(aux)) senseList.Add(aux);
        }

        public void addStateActTransition(string rule)
        {
            // "state|act=prob:nextstate|nextstate|..."
            string[] parms = rule.Split('=');
            string key = parms[0];
            string[] keySplit = key.Split('|');
            string state = keySplit[0];
            string act = keySplit[1];

            string strResult = parms[1];
            string[] resultSplit = strResult.Split(':');
            
            string strProb = resultSplit[0];
            double prob = double.Parse(strProb);
            string strNextList = resultSplit[1];
            string[] nextList = strNextList.Split('|');

            if (!state_act_tansition.ContainsKey(key))
            {
                state_act_tansition.Add(key, new Estimator());
            }
            Estimator myEstimate = state_act_tansition[key];
            myEstimate.rule = rule;
            myEstimate.state = state;
            myEstimate.aux = act;
            foreach (string nextState in nextList)
            {
                if (myEstimate.estimate.ContainsKey(nextState))
                {
                    myEstimate.estimate[nextState] = prob;
                }
                else
                {
                    myEstimate.estimate.Add(nextState, prob);
                }
                if (!stateList.Contains(nextState)) stateList.Add(nextState);
            }
            if (!stateList.Contains(state)) stateList.Add(state);
            if (!actList.Contains(act)) actList.Add(act);

        }
        // We're building a frame slot/value system
        // where the frame is indexed as "the world I am experiencing now"
        // Or like Horswill's role passing system, with the particles as
        // "the world I am tracking"
        public double probParticleGeneratedObservation(Observation obsrv, Particle p)
        {
            double prob = 1;
            string[] statekeys = p.variables.Keys.ToArray();
            foreach (string state in statekeys)
            {
                double stateP = p.variables[state];
                double pgen = 1;
                if (stateP < minimumP) stateP = minimumP;
                foreach (string sense in obsrv.observable.Keys)
                {
                    double senseP = obsrv.observable[sense];
                    string key = String.Format("{0}|{1}", state, sense);
                    if (state_sense_prob.ContainsKey(key))
                    {
                        Estimator myEstimate = state_sense_prob[key];
                        foreach (string facet in myEstimate.estimate.Keys)
                        {
                            // should we overwrite or just multiply mix in ?
                            //p.variables[facet] =  p.variables[facet]* myEstimate.estimate[facet];
                            pgen = pgen * (myEstimate.estimate[facet] * senseP);

                        }
                        // Then what ???
                    }
                    else
                    {
                        Console.WriteLine("Not found:'{0}'", key);
                    }
                }
                double finP = stateP * pgen;
                p.variables[state] = finP;
                //Console.WriteLine("obv.state({0}) = {1}", state, finP);
               // p.variables[state] =  pgen;
            }
            // Do the final conjunction of particle facets computation ?
            prob = minimumP ;
            foreach (string facet in statekeys)
            {
               //prob=prob* p.variables[facet];
                if (p.variables[facet] > prob) prob = p.variables[facet];
            }
            //Console.WriteLine("fp={0}\n", prob);
            return prob;
        }

        public Particle defaultTransformParticle(Particle p)
        {
            return transformParticle(inputAct, p);
        }

        public Particle transformParticle (Action act, Particle p)
        {
            //Particle nextParticle = new Particle(p);
            Particle nextParticle = new Particle();
            //nextParticle.variables = p.variables;
            nextParticle.prob = p.prob;
            double maxProb = minimumP;
            string[] statekeys = p.variables.Keys.ToArray();
            foreach (string state in statekeys)
            {
                double stateP = p.variables[state];
                if (stateP < minimumP) stateP = minimumP;
                foreach (string actv in act.actParms.Keys)
                {
                    double actP = act.actParms[actv];
                    string key = String.Format("{0}|{1}", state, actv);
                    if (state_act_tansition.ContainsKey(key))
                    {
                        Estimator myEstimate = state_act_tansition[key];
                        // fill in each facet
                        foreach (string facet in myEstimate.estimate.Keys)
                        {
                            double estP = (myEstimate.estimate[facet] * stateP * actP);
                            double noise = (estP*0.01 * rgen.NextDouble()) - (estP*0.005);
                            estP = Math.Abs(estP + noise);
                            //double estP = (myEstimate.estimate[facet] * stateP) ;
                            if ((!nextParticle .variables .ContainsKey (facet )) ||(estP > nextParticle.variables[facet]))
                            {
                                nextParticle.variables[facet] =  estP;
                                nextParticle.prob = estP;
                            }
                            if (estP > maxProb) maxProb = estP;
                            // Then what ???
                        }
                    }
                    else
                    {
                        Console.WriteLine("Not found:'{0}'", key);
                    }

                }
            }
            if (maxProb < minimumP) maxProb = minimumP;
            nextParticle.prob = maxProb; // go with the strongest frame facet for now
            if (maxProb == 0)
            {
                Console.WriteLine("maxProb==0");
            }
            return nextParticle;

        }

        public void start( List<Observation> observations)
        {
            particles = new Particle[numberOfParticles];
            accum = new double[numberOfParticles];
            foreach (Observation observation in observations)
            {
                performLikelihoodWeighting(observation);
                
                foreach (Particle p in particles)
                {
                    p.prob = observation.getLikelihood(p);
                    //do you want to compute mean/variance for each variable ?
                }
            }
        }
        public void fillRandomVars(Particle p)
        {
            foreach (string k in prototype.variables.Keys)
            {
                //double noise = (0.01 * rgen.NextDouble()) - 0.005;

                p.variables[k] = (rgen.NextDouble() * prototype.variables[k]);
               // p.variables[k] = (1 / (1 + prototype.variables.Count));
            }

        }

        public void performSystemTransition()
        {
            if (processParticleTransform == null)
            {
                Console.WriteLine("WARNING: NO processParticleTransform defined!");
                return;
            }
            // Move the set of particles forward
            Particle[] newParticleSet = new Particle[numberOfParticles];

            for (int i = 0; i < numberOfParticles; i++)
            {
                newParticleSet[i] = processParticleTransform(particles[i]);
            }
            particles = newParticleSet;

        }

        public void resample()
        {
            int method = 1;

            //double totalLikelihood = 0;
            //for (int i = 0; i < numberOfParticles; i++)
            //{
            //    totalLikelihood += particles[i].prob;
            //    accum[i] = totalLikelihood;
            //}
            normalizeWeighting();
            Particle[] newParticleSet = new Particle[numberOfParticles];

            if (method == 0)
            {
                // One method is to estimate how many copies should be 
                // made for each particle and fill in the remainder with random
                // to keep convergence 
                // (could also do random sample)
                // Which should be O(n)

                int copyPoint = 0;
                for (int i = 0; i < numberOfParticles; i++)
                {
                    int numToCopy = (int)(numberOfParticles * (particles[i].prob / totalLikelihood));
                    for (int j = 0; j < numToCopy; j++)
                    {
                        newParticleSet[copyPoint++] = particles[i];
                    }
                }
                if (copyPoint < numberOfParticles)
                {
                    for (int i = copyPoint; i < numberOfParticles; i++)
                    {
                        if (rgen.NextDouble() < 0.5)
                        {
                            // the new random particle option
                            newParticleSet[i] = new Particle();
                            fillRandomVars(newParticleSet[i]);
                        }
                        else
                        {
                            // the copy random existing particle option
                            int rp = rgen.Next(0, numberOfParticles);
                            newParticleSet[i] = particles[rp];
                        }
                    }
                }
            }
            if (method == 1)
            {
                // using a binary search with O(n)+O(n log n) instead of 
                // normal linear accum search for O(n*n)
                for (int i = 0; i < numberOfParticles; i++)
                {
                    double rnd = rgen.NextDouble() * totalWeight;
                    if (rnd < accum[0]) rnd = accum[0];
                    if (rnd > accum[numberOfParticles - 1]) rnd = accum[numberOfParticles - 1];
                    int j = Array.BinarySearch(accum, rnd);
                    if (j < 0)
                    {
                        j = ~j;
                        j = j - 1;
                    }
                    newParticleSet[i] = particles[j];
                    newParticleSet[i].normalize(constraintSet);
                }
            }
            particles = newParticleSet;

        }
        public void normalizeWeighting()
        {
            totalLikelihood = 0;
            totalWeight = 0;
            for (int i = 0; i < numberOfParticles; i++)
            {
                totalLikelihood += particles[i].prob;
                //accum[i] = totalLikelihood;
            }
            if (totalLikelihood == 0)
            {
                for (int i = 0; i < numberOfParticles; i++)
                {
                    particles[i].weight = rgen.NextDouble()*0.0001;
                    totalWeight += particles[i].weight;
                    accum[i] = totalWeight;
                }
            }
            else
            {
                for (int i = 0; i < numberOfParticles; i++)
                {
                    particles[i].weight = particles[i].prob / totalLikelihood;
                    totalWeight += particles[i].weight;
                    accum[i] = totalWeight;
                }
            }
            if (double.IsNaN(totalLikelihood))
            {
                Console.WriteLine("NaN totalLikelihood ERR!");
            }
            if (double.IsNaN(totalWeight))
            {
                Console.WriteLine("NaN totalWeight ERR!");
            }

        }
        public void performLikelihoodWeighting(Observation obv)
        {
            if (!initialized)
            {
                // Fill with random priors
                for (int i = 0; i < numberOfParticles; i++)
                {
                    particles[i] = new Particle();
                    fillRandomVars(particles[i]);
                }
                initialized = true;
            }
            // Preform action first
            performSystemTransition();
            // Then check the observation
            for (int i = 0; i < numberOfParticles; i++)
            {
                ///double likelihood = obv.getLikelihood(particles[i], probParticleGeneratedObservation);
                double likelihood = probParticleGeneratedObservation(obv, particles[i]);
               // particles[i].prob = particles[i].prob * likelihood;
                particles[i].prob = likelihood;
                if (double.IsNaN(likelihood))
                {
                    Console.WriteLine("NaN likelihood ERR!");
                }
            }
            //Resample on the final 
            resample();
        }

        //Normal operation:
        // 1. initilize distribution (particle set)
        // 2. observe system and find prob for each particle based on observation
        // 3. Normalize the particle weights
        // 4. Resample the distribution to get a new distribution
        //     select particle proportinal to importance weight
        // 5. Add noise to filter
        // 6. predict how system advances through time, and update particles
        // 7. goto 2

        public void updateModel(Observation obv)
        {
            performLikelihoodWeighting(obv);
        }

        public void quickFilter(string actionList, string observationList)
        {
            Console.WriteLine("QF: {0} , {1}", actionList, observationList);

            string [] actArry = actionList.Split('|');
            string [] obsArry = observationList.Split('|');

            inputAct = new Action();
            inputObservation = new Observation();

            foreach (string a in actArry)
            {
                inputAct.actParms.Add(a, 1.0);
            }
            foreach (string o in obsArry)
            {
                inputObservation.observable.Add(o, 1.0);
            }
            updateModel(inputObservation);
            if (traceStack < traceDepth-1)
            {
                traceStack++;
            }
            else
            {
                for (int i = 0; i < traceDepth; i++)
                {
                    traceSet[i] = traceSet[i + 1];
                }
            }
            defMeanParticle ();
            traceSet[traceStack] = new Particle (meanParticle) ;
        }
        public void dump()
        {
            for (int i = 0; i < numberOfParticles; i++)
            {
                Console .WriteLine ("{0} : {1}",i,particles [i].ToString ());
            }
        }
    }
}
