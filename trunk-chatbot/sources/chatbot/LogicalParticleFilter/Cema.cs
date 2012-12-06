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
        public string idCode="";

        public List<string> modList;
        public List<string> missingList;
        public List<string> violationList;

        public double f()
        {
            return costSoFar() + distToGoal();
        }
        public double distToGoal()
        {
            // simple heuristic : number of missing elements to add + number of undesired to remove
            if (missingList == null) return 0;
            return missingList.Count+violationList.Count;
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
            modList.Sort();
            lock (idCode)
            {
                idCode = "";
                foreach (string m in modList)
                {
                    if (idCode.Length > 0) idCode += "|";
                    idCode += m;
                }
            }
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
        // TODO's:
        // + implement avoids(x) - a solution must NOT provide(x)
        //    - would bring closer to SAT representational capability
        // + sort modules and concat to function as a unique solution key
        // + raw query's,  without explicit 'provides' check of just propositions
        // + return BTXML fragment representing plan

        SIProlog prologEngine = null;
        public bool worstWeighting = false;
        public bool nondeterministic = false;
        public double problemWorstCost = -1;
        public double limitCost = double.MaxValue;
        public int limitTrials = int.MaxValue;
        public int tickBegin = 0;
        public int tickEnd = 0;
        public int trials = 0;
        List<CemaState> closedSet = new List<CemaState>();
        List<CemaState> openSet = new List<CemaState>();
        CemaState planNode = null;

        public CemaSolver(SIProlog prologEng)
        {
            prologEngine = prologEng;
        }
        public bool isPrologish(string code)
        {
            // quick test for a functor syntax which could be directly queried
            return ((code.Contains("(")) && (code.Contains(")")));
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
                if (isPrologish(need))
                {
                    string needQuery =need.Replace('"',' ');
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, proposalMt);
                    if (!needSatisfied)
                    {
                        if (!missingList.Contains(need))
                            missingList.Add(need);
                    }

                }
                else
                {
                    string needQuery = String.Format("provides({0})", need);
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, proposalMt);
                    if (!needSatisfied)
                    {
                        if (!missingList.Contains(need))
                            missingList.Add(need);
                    }
                }
            }
            return missingList;
        }

        public List<string> violationsInMt(string proposalMt)
        {
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            // Find Desired List
            string reqQuery = "avoid(CONSTRAINT)";
            List<string> constraintList = new List<string>();
            prologEngine.askQuery(reqQuery, proposalMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "CONSTRAINT")
                    {
                        if (!constraintList.Contains(bindings[k])) constraintList.Add(bindings[k]);
                    }
                }
            }
            if (constraintList.Count == 0) return new List<string>();
            // Find out what is missing
            List<string> violationList = new List<string>();
            foreach (string constraint in constraintList)
            {
                if (isPrologish(constraint))
                {
                    string constraintQuery = constraint.Replace('"', ' '); ;
                    bool constraintViolated = prologEngine.isTrueIn(constraintQuery, proposalMt);
                    if (constraintViolated)
                    {
                        if (!violationList.Contains(constraint))
                            violationList.Add(constraint);
                    }

                }
                else
                {
                    string constraintQuery = String.Format("provides({0})", constraint);
                    bool constraintViolated = prologEngine.isTrueIn(constraintQuery, proposalMt);
                    if (constraintViolated)
                    {
                        if (!violationList.Contains(constraint))
                            violationList.Add(constraint);
                    }
                }
            }
            return violationList;
        }

        public bool isRelevantMt(string moduleMt, List<string> needList)
        {
            foreach (string need in needList)
            {
                if (isPrologish(need))
                {
                    string needQuery = need.Replace('"', ' '); ;
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, moduleMt);
                    if (needSatisfied) return true;
                }
                else
                {
                    string needQuery = String.Format("provides({0})", need);
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, moduleMt);
                    if (needSatisfied) return true;
                }
            }
            return false;
        }

        public bool isViolatingMt(string moduleMt, List<string> avoidList)
        {
            foreach (string negConstraint in avoidList)
            {
                if (isPrologish(negConstraint))
                {
                    string violationQuery = negConstraint.Replace('"', ' '); ;
                    bool violatesConstraint = prologEngine.isTrueIn(violationQuery, moduleMt);
                    if (violatesConstraint) return true;
                }
                else
                {
                    string violationQuery = String.Format("provides({0})", negConstraint);
                    bool violatesConstraint = prologEngine.isTrueIn(violationQuery, moduleMt);
                    if (violatesConstraint) return true;
                }

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
            tickBegin = Environment.TickCount;
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
            List<string> violationList = violationsInMt(problemMt);

            CemaState start = new CemaState(new List<string>(), missingList);
            // get initial Eval
            setSolution(start, solutionMt, problemMt);
            if ((missingList.Count == 0) &&(violationList .Count ==0))
            {
                commitSolution(start, solutionMt, problemMt);
                return true; // nothing is missing so done
            }

           closedSet = new List<CemaState>();
           openSet = new List<CemaState>();

            //cost expended so far
            Dictionary<string, double> gScores = new Dictionary<string, double>();

            //Estimate how far to go
            Dictionary<string, double> hScores = new Dictionary<string, double>();

            //combined f(n) = g(n)+h(n)
            Dictionary<string, double> fScores = new Dictionary<string, double>();

            gScores.Add(start.idCode , 0);
            hScores.Add(start.idCode, start.distToGoal() * problemWorstCost);
            fScores.Add(start.idCode, (gScores[start.idCode] + hScores[start.idCode]));
            
            openSet.Add(start);
            trials = 0;
            while (openSet.Count != 0)
            {
                trials++;
                if (trials > limitTrials) break;

                //we look for the node within the openSet with the lowest f score.
                CemaState bestState = this.FindBest(openSet, fScores, nondeterministic);
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
                        gScores[nextState.idCode] = nextState.costSoFar();
                        hScores[nextState.idCode] = nextState.distToGoal() * problemWorstCost;
                        fScores[nextState.idCode] = (gScores[nextState.idCode] + hScores[nextState.idCode]);
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
            List<string> solutionViolationList = violationsInMt(problemMt);

            cState.missingList = solutionMissingList;
            cState.violationList = solutionViolationList;
        }

        public void commitSolution(CemaState cState, string solutionMt, string problemMt)
        {
            planNode = cState;
            // Post stats and planner state
            string postScript = "";
            postScript += String.Format("g({0}).\n", cState.costSoFar());
            postScript += String.Format("h({0}).\n", cState.distToGoal());
            postScript += String.Format("f({0}).\n", cState.costSoFar() + cState.distToGoal() * problemWorstCost);
            postScript += String.Format("worst({0}).\n", problemWorstCost);
            postScript += String.Format("openedNodes({0}).\n", openSet.Count);
            postScript += String.Format("closedNodes({0}).\n", closedSet.Count);
            postScript += String.Format("totalNodes({0}).\n", openSet.Count + closedSet.Count);

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
            string modString = "";
            if (cState.modList.Count > 0)
            {
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
            tickEnd = Environment.TickCount;
            int elapsed = tickEnd - tickBegin;
            int totalNodes= openSet.Count + closedSet.Count;
            Console.WriteLine("Inventing time = {0}", elapsed);
            Console.WriteLine("Inventing list = {0}", modString);
            
            Console.WriteLine("Inventing tials = {0}", trials);
            Console.WriteLine("TotalNodes = {0}", totalNodes);
            if (trials > 0)
            {
                Console.WriteLine("Inventing ms/trials = {0}", ((double)elapsed / (double)trials));
            }
            if (totalNodes > 0)
            {
                double mspn = ((double)elapsed / (double)totalNodes);
                Console.WriteLine("Inventing ms/nodes = {0}", mspn);
                if (mspn > 0)
                {
                    Console.WriteLine("Inventing @ nodes/sec = {0}", 1000 / mspn);
                }
            }
            if (elapsed > 0)
            {
                Console.WriteLine("Inventing trials/ms = {0}", ((double)trials / (double)elapsed));
                Console.WriteLine("Inventing nodes/ms = {0}", ((double)totalNodes / (double)elapsed));
            }

            Console.WriteLine(postScript);

        }

        /// <summary>
        /// Returns an BTXML code fragment with the action/modules as subbehaviors
        /// </summary>
        /// <param name="outerTag">The outer tag to wrap the call in (selector/sequence/...)</param>
        /// <returns></returns>        
        /// 
        public string getBTXMLFragment(string outerTag)
        {
            if (planNode == null) return "";

            if (planNode.modList.Count > 0)
            {
                string planSequence = "";
                foreach (string m in planNode.modList)
                {
                    planSequence += String.Format("  <subbehavior id='{0}'/>\n", m);
                }
                string finalCode = String.Format("<{0}>\n{1}</{0}>\n", outerTag, planSequence);
                return finalCode;
            }
            else
            {
                return "";
            }
        }

        /// <summary>
        /// Returns an BTXML behavior definition with the action/modules as subbehaviors
        /// </summary>
        /// <param name="behaviorID">Name of the behavior to create</param>
        /// <param name="outerTag">The outer tag to wrap the call in (selector/sequence/...)</param>
        /// <returns></returns>        
        /// 
        public string getBTXMLBehaviorCode(string behaviorID, string outerTag)
        {
            string innerCode = getBTXMLFragment(outerTag);
           string behaviorCode = String.Format("<behavior id='{0}'>\n{1}</behavior>\n", behaviorID);
           return behaviorCode;
        }

        /// <summary>
        /// Finds the state with the lowest value in fScores
        /// </summary>
        /// <param name="set">A list of CemaStates</param>
        /// <param name="fScores">A dictionary of CemaStates and their fScores</param>
        /// <returns></returns>
        private CemaState FindBest(List<CemaState> set, Dictionary<string, double> fScores,bool randomBest)
        {
            CemaState lowestState = null;
            double lowest = double.MaxValue;
            int bestCount = 1;
            //loop through all states in the list
            foreach (CemaState state in set)
            {

                double value = fScores[state.idCode];

                if ((value == lowest) && (randomBest))
                {
                    bestCount++;
                    Random rnd = new Random();
                    if (rnd.NextDouble() < (1 / (double)bestCount))
                    {
                        lowestState = state;
                        lowest = value;
                    }
                }
                //keep the best score
                if (value < lowest)
                {
                    lowestState = state;
                    lowest = value;
                    bestCount = 1;
                }

            }

            return lowestState;
        }

    }
}
