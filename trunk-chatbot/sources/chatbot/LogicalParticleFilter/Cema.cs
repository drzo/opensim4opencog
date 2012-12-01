using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// CEMA mt composition planner -- Copyright (c) 2012,Kino Coursey, Daxtron Labs
// License: BSD

namespace LogicalParticleFilter1
{
    public class CemaState:IComparable <CemaState>
    {
        public double totalCost=0;
        public List<string> modList;
        public List<string> missingList;
        public double f()
        {
            return costSoFar() + distToGoal();
        }
        public double distToGoal()
        {
            if (missingList == null) return 0;
            return missingList.Count;
        }
        public double costSoFar()
        {
            if (totalCost > 0) return totalCost;
            if (modList == null) return 0;
            return modList.Count;
        }
        public CemaState(List<string> inModList, List<string> inMissingList)
        {
            modList = inModList;
            missingList = inMissingList;
            if (inModList == null) modList = new List<string>();
            if (inMissingList == null) missingList = new List<string>();
        }
        public  List <string> validNextMods(List<string> inModList)
        {
            List <string> vlist=new List<string> ();
            foreach (string mod in inModList )
            {
                if (!modList.Contains(mod))
                {
                        vlist.Add(mod);
                }
            }
            return vlist;
        }
        public int CompareTo(CemaState theOtherNode)
        {
            double theResult = f().CompareTo( theOtherNode.f());

            if (theResult < 0)
                return -1;
            else if (theResult > 0)
                return 1;
            else
                return 0;
        }
    }

    public class CemaSolver
    {
        // CEMA: Condensed End-Means Analysis
        // A simple additive dependency planner where each MT is a module
        // and it searchs the space of module combinations until no requirement
        // goes unmet. CEMA has many descriptions but this one will be over
        // propositions defined in the predicates of the relevant Mt's.
        // Using an A* search with 
        //   h(n)= number of unmet conditions
        //   g(n)= number of modules used so far
        //   f(n) = h(n)+g(n)
        // note: g(n) could be defined by the sum of a cost predicate in each module

        // Requires 
        // - an MT defining the problem spec
        // - an MT having all module Mt's visible
        // - a set of module mt's containing
        //    - module(module_mt_name)
        //    - optionally cost(module_cost)
        //    - set/list of requires(proposition)
        //    - set/list of provides(proposition)
        //    - any other information that defines that module
        // - System will return 
        //    - a list of module mt's that provide a solution
        //    - a solution mt with a genlMt to all the solution modules
        // Note: solution mt local contents will be overwritten on each invocation

        SIProlog prologEngine = null;
        public bool worstWeighting = false;
        public double problemWorstCost = -1;
        public double limitCost = double.MaxValue;
        public int limitTrials = int.MaxValue;


        public CemaSolver(SIProlog prologEng)
        {
            prologEngine = prologEng;
        }

        public List<string> missingInMt(string proposalMt)
        {
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            // Find Desired List
            string reqQuery = "requires(NEED)";
            List<string> needList = new List<string>();
            prologEngine.askQuery(reqQuery, proposalMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "NEED")
                    {
                        if (!needList.Contains(bindings[k])) needList.Add(bindings[k]);
                    }
                }
            }
            if (needList.Count==0) return new List<string> ();
            // Find out what is missing
            List<string> missingList = new List<string>();
            foreach (string need in needList)
            {
                string needQuery = String.Format("provides({0})", need);
                bool needSatisfied = prologEngine.isTrueIn (needQuery, proposalMt);
                if (!needSatisfied)
                {
                  if (!missingList.Contains (need))
                      missingList.Add(need);
                }
            }
            return missingList;
        }
        public bool isRelevantMt(string moduleMt, List<string> needList)
        {
            foreach (string need in needList)
            {
                string needQuery = String.Format("provides({0})", need);
                bool needSatisfied = prologEngine.isTrueIn(needQuery, moduleMt);
                if (needSatisfied) return true;
            }
            return false;
        }

        public double getModuleCost(string moduleMt)
        {
            // be pessimistic on the module cost
            // if no cost found then assume a step of +1
            // all costs should be positive and found in the module mt

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            string costQuery = "cost(COST)";
            prologEngine.askQuery(costQuery, moduleMt, out bingingsList);
            double worstCost = -1;
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "COST")
                    {
                        double newCost = double.Parse (bindings [k].Trim());
                        if (newCost > worstCost)
                        {
                            worstCost = newCost;
                        }
                    }
                }
            }
            if (worstCost == -1)
            {
                worstCost = 1;
            }
            return worstCost;

        }

        public bool constructSolution(string problemMt,string moduleMt, string solutionMt)
        {
            // CEMA
            prologEngine.connectMT(solutionMt, problemMt);
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            
            List<string> totalModuleList = new List<string>();

            // Collect Module List
            string query = "module(MODMT)";
            prologEngine.askQuery(query, moduleMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "MODMT") totalModuleList.Add(bindings[k]);
                }
            }

            // Find worst cost
            // h(n)*problemWorstCost should be admissible for A*
            problemWorstCost = -1;

            if (worstWeighting)
            {
                string costQuery = "cost(COST)";
                prologEngine.askQuery(costQuery, moduleMt, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    foreach (string k in bindings.Keys)
                    {
                        if (k == "COST")
                        {
                            double newCost = double.Parse(bindings[k].Trim());
                            if (newCost > problemWorstCost)
                            {
                                problemWorstCost = newCost;
                            }
                        }
                    }
                }
                if (problemWorstCost == -1)
                {
                    problemWorstCost = 1;
                }
            }
            else
            {
                problemWorstCost = 1;
            }


            List<string> missingList = missingInMt(problemMt);
            CemaState start = new CemaState(new List<string>(), missingList);
            // get initial Eval
            setSolution(start, solutionMt, problemMt);
            if (missingList.Count == 0)
            {
                commitSolution(start, solutionMt, problemMt);
                return true; // nothing is missing so done
            }
            List<CemaState> closedSet = new List<CemaState>();
            List<CemaState> openSet = new List<CemaState>();

            //cost expended so far
            Dictionary<CemaState, double> gScores = new Dictionary<CemaState, double>();

            //Estimate how far to go
            Dictionary<CemaState, double> hScores = new Dictionary<CemaState, double>();

            //combined f(n) = g(n)+h(n)
            Dictionary<CemaState, double> fScores = new Dictionary<CemaState, double>();

            gScores.Add(start, 0);
            hScores.Add(start, start.distToGoal() * problemWorstCost);
            fScores.Add(start, (gScores[start] + hScores[start]));
            
            openSet.Add(start);
            int trials = 0;
            while (openSet.Count != 0)
            {
                trials++;
                if (trials > limitTrials) break;

                //we look for the node within the openSet with the lowest f score.
                CemaState bestState = this.FindBest(openSet, fScores);
                setSolution(bestState, solutionMt,problemMt );

                // if goal then we're done
                if (bestState.distToGoal() == 0)
                {
                    // return with the solutionMt already connected
                    commitSolution(bestState, solutionMt, problemMt);
                    return true;
                }
                openSet.Remove(bestState);
                closedSet.Add(bestState);

                // Not the final solution and too expensive
                if (bestState.totalCost > limitCost)
                    continue;

                // get the list of modules we have not used
                List<string> validModules = bestState.validNextMods(totalModuleList);
                foreach (string nextModule in validModules)
                {
                    // only consider those that provide something missing
                    if (!isRelevantMt(nextModule, bestState.missingList))
                        continue;

                    double nextCost = getModuleCost(nextModule);

                    // Ok nextModule is relevant so clone bestState and extend
                    List <string>nextModList = new List<string> ();
                    foreach(string m in bestState .modList ) nextModList.Add(m);
                    nextModList.Add(nextModule);

                    CemaState nextState = new CemaState(nextModList, null);
                    nextState.totalCost = bestState.totalCost + nextCost;
                    // measure the quality of the next state
                    setSolution(nextState, solutionMt, problemMt);

                    //skip if it has been examined
                    if (closedSet.Contains(nextState))
                        continue;

                    if (!openSet.Contains(nextState))
                    {
                        openSet.Add(nextState);
                        gScores.Add(nextState, nextState.costSoFar ());
                        hScores.Add(nextState, nextState.distToGoal() * problemWorstCost);
                        fScores.Add(nextState, (gScores[nextState] + hScores[nextState]));
                    }
                }
                openSet.Sort();
            }
            // an impossible task appently
            commitSolution(start, solutionMt, problemMt);
            return false;
        }


        public void setSolution(CemaState cState, string solutionMt,string problemMt)
        {
            // Make the description in cState the focus
            prologEngine.clearKB(solutionMt);
            prologEngine.clearConnectionsFromMt(solutionMt);
            prologEngine.connectMT(solutionMt, problemMt);
            foreach (string moduleMt in cState.modList)
            {
                prologEngine.connectMT(solutionMt, moduleMt);
            }
            List<string> solutionMissingList = missingInMt(solutionMt);
            cState.missingList = solutionMissingList;
        }

        public void commitSolution(CemaState cState, string solutionMt, string problemMt)
        {
            // Post stats and planner state
            string postScript = "";
            postScript += String.Format("g({0}).\n", cState.costSoFar());
            postScript += String.Format("h({0}).\n", cState.distToGoal());
            postScript += String.Format("f({0}).\n", cState.costSoFar() + cState.distToGoal() * problemWorstCost);
            postScript += String.Format("worst({0}).\n", problemWorstCost);

            if (cState.distToGoal() == 0)
            {
                postScript += "planstate(solved).\n";
            }
            else
            {
                postScript += "planstate(unsolved).\n";

            }

            prologEngine.appendKB(postScript,solutionMt);

            // post the modules used
            if (cState.modList.Count > 0)
            {
                string modString = "";
                foreach (string m in cState.modList)
                {
                    modString += " " + m;
                }
                prologEngine.appendListPredToMt("modlist", modString, solutionMt);
            }
            else
            {
                prologEngine.appendKB("modlist([]).\n", solutionMt);
            }
            //post anything missing.

            if (cState.missingList.Count > 0)
            {
                string missingString = "";
                foreach (string m in cState.missingList)
                {
                    missingString += " " + m;
                }
                prologEngine.appendListPredToMt("missing", missingString, solutionMt);
            }
            else
            {
                prologEngine.appendKB("missing([]).\n", solutionMt);
            }

        }
        /// <summary>
        /// Finds the state with the lowest value in fScores
        /// </summary>
        /// <param name="set">A list of CemaStates</param>
        /// <param name="fScores">A dictionary of CemaStates and their fScores</param>
        /// <returns></returns>
        private CemaState FindBest(List<CemaState> set, Dictionary<CemaState, double> fScores)
        {
            CemaState lowestState = null;
            double lowest = double.MaxValue;

            //loop through all states in the list
            foreach (CemaState state in set)
            {

                double value = fScores[state];

                //keep the best score
                if (value < lowest)
                {
                    lowestState = state;
                    lowest = fScores[state];
                }
            }

            return lowestState;
        }

    }
}
