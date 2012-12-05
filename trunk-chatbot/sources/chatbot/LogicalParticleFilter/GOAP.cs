using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

//GOAP Goal oriented action planing using MT's -- Copyright (c) 2012,Kino Coursey, Daxtron Labs
// License: BSD

namespace LogicalParticleFilter1
{
    public class GoapState : IComparable<GoapState>
    {
        public double totalCost = 0;
        public string idCode = "";

        public List<string> modList; // our module/action sequence
        public List<string> missingList; // states unmet
        //public List<string> violationList;

        public double f()
        {
            return costSoFar() + distToGoal();
        }
        public double distToGoal()
        {
            // simple heuristic : number of missing elements to add + number of undesired to remove
            if (missingList == null) return 0;
            return missingList.Count;// +violationList.Count;
        }
        public double costSoFar()
        {
            if (totalCost > 0) return totalCost;
            if (modList == null) return 0;
            return modList.Count;
        }

        public GoapState(List<string> inModList, List<string> inMissingList)
        {
            modList = inModList;
            missingList = inMissingList;
            if (inModList == null) modList = new List<string>();
            if (inMissingList == null) missingList = new List<string>();
            missingList.Sort();
            lock (idCode)
            {
                idCode = "";
                foreach (string m in missingList)
                {
                    if (idCode.Length > 0) idCode += "|";
                    idCode += m;
                }
            }
        }

        public List<string> validNextMods(List<string> inModList)
        {
            List<string> vlist = new List<string>();
            foreach (string mod in inModList)
            {
                //if (!modList.Contains(mod))
                {
                    vlist.Add(mod);
                }
            }
            return vlist;
        }
        public int CompareTo(GoapState theOtherNode)
        {
            double theResult = f().CompareTo(theOtherNode.f());

            if (theResult < 0)
                return -1;
            else if (theResult > 0)
                return 1;
            else
                return 0;
        }
    }


    public class GOAPSolver
    {

        SIProlog prologEngine = null;
        public bool worstWeighting = false;
        public bool nondeterministic = false;
        public double problemWorstCost = -1;
        public double limitCost = double.MaxValue;
        public int limitTrials = int.MaxValue;
        public int tickBegin = 0;
        public int tickEnd = 0;
        public int trials = 0;
        List<GoapState> closedSet = new List<GoapState>();
        List<GoapState> openSet = new List<GoapState>();
        GoapState planNode = null;

        public GOAPSolver(SIProlog prologEng)
        {
            prologEngine = prologEng;
        }
        public bool isPrologish(string code)
        {
            // quick test for a functor syntax which could be directly queried
            return ((code.Contains("(")) && (code.Contains(")")));
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
                        double newCost = double.Parse(bindings[k].Trim());
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

        public List<string> missingInMt(string goalMt, string nowMt)
        {
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            // Find Desired List
            string reqQuery = "precond(NEED)";
            List<string> needList = new List<string>();
            prologEngine.askQuery(reqQuery, goalMt, out bingingsList);
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
            if (needList.Count == 0) return new List<string>();
            // Find out what is missing
            List<string> missingList = new List<string>();
            foreach (string need in needList)
            {
                if (isPrologish(need))
                {
                    string needQuery = need.Replace('"', ' '); ;
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, nowMt);
                    if (!needSatisfied)
                    {
                        if (!missingList.Contains(need))
                            missingList.Add(need);
                    }
                }
                else
                {
                    string needQuery = String.Format("state({0})", need);
                    bool needSatisfied = prologEngine.isTrueIn(needQuery, nowMt);
                    if (!needSatisfied)
                    {
                        if (!missingList.Contains(need))
                            missingList.Add(need);
                    }
                }
            }
            return missingList;
        }

        public bool isRelevantMt(string moduleMt, List<string> needList)
        {
            // does action Mt have an effect for any of the unmet needs ?
            foreach (string need in needList)
            {
                string needQuery = String.Format("effect({0})", need);
                bool needSatisfied = prologEngine.isTrueIn(needQuery, moduleMt);
                if (needSatisfied) return true;
            }
            return false;
        }

        public List<string> transformAct(string moduleMt, List<string> needList)
        {
            // merge existing need list + module preconditions
            List<string> combinedNeeds = new List<string>();
            foreach (string n in needList) { combinedNeeds.Add(n); }
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            string reqQuery = "precond(NEED)";
            prologEngine.askQuery(reqQuery, moduleMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "NEED")
                    {
                        if (!combinedNeeds.Contains(bindings[k])) combinedNeeds.Add(bindings[k]);
                    }
                }
            }
          // apply effects
            List<string> effectList = new List<string>();

            string effectQuery = "effect(RESULT)";
            prologEngine.askQuery(effectQuery, moduleMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "RESULT")
                    {
                        if (!effectList.Contains(bindings[k])) effectList.Add(bindings[k]);
                    }
                }
            }
         // remove those unmet needs that an effect addresses
            foreach (string e in effectList)
            {
                if (combinedNeeds.Contains(e))
                {
                    combinedNeeds.Remove(e);
                }
            }
          // return the conditions that are still UNMET
            return combinedNeeds;
        }

        // GOAP is different from CEMA in:
        // - backward planning process looking for a defined nowMt
        // - sequence of action modules matters
        // - "state" is the set of unmet goal conditions
        // - a goal state has no conditions in it

        // In Mt's:
        //   state(prop) : prop is true
        //   precond(prop) : is a goal to make true
        //   effect(prop) : applying mt will make prop true
        //   cost(n) : using mt will cost n, default is 1

        // Using an A* search with 
        //   h(n)= number of unmet conditions
        //   g(n)= number of modules used so far
        //   f(n) = h(n)+g(n)
        // note: g(n) could be defined by the sum of a cost predicate in each module

        // Requires 
        // - an MT defining the goal state spec using precond(p)
        // - an MT defining the current state using state(p)
        // - an MT defining the background context logic if any
        // - an MT having all module Mt's visible
        // - a set of module mt's containing
        //    - module(module_mt_name)
        //    - optionally cost(module_cost)
        //    - set/list of precond(proposition)
        //    - set/list of effect(proposition)
        //    - any other information that defines that module
       
        // - System will return 
        //    - a sequence of module mt's that provide a solution
        //    - a solution mt with a genlMt to all the solution modules

        // TODO's:
        // + return BTXML fragment representing plan
        // - possible single first action for replanning agents

        public bool constructPlan(string goalMt,string nowMt, string moduleMt,string backgroundMt, string solutionMt)
        {
            tickBegin = Environment.TickCount;

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

            List<string> missingList = missingInMt(goalMt,nowMt);

            GoapState start = new GoapState(new List<string>(), missingList);
            // get initial Eval
            setSolution(start, solutionMt, nowMt, backgroundMt);

            if ((missingList.Count == 0))
            {
                commitSolution(start, solutionMt, nowMt,backgroundMt );
                return true; // nothing is missing so done
            }

            closedSet = new List<GoapState>();
            openSet = new List<GoapState>();

            //cost expended so far
            Dictionary<string, double> gScores = new Dictionary<string, double>();

            //Estimate how far to go
            Dictionary<string, double> hScores = new Dictionary<string, double>();

            //combined f(n) = g(n)+h(n)
            Dictionary<string, double> fScores = new Dictionary<string, double>();

            gScores.Add(start.idCode, 0);
            hScores.Add(start.idCode, start.distToGoal() * problemWorstCost);
            fScores.Add(start.idCode, (gScores[start.idCode] + hScores[start.idCode]));

            openSet.Add(start);
            trials = 0;

            while (openSet.Count != 0)
            {
                trials++;
                if (trials > limitTrials) break;

                //we look for the node within the openSet with the lowest f score.
                GoapState bestState = this.FindBest(openSet, fScores, nondeterministic);
                setSolution(bestState, solutionMt, nowMt, backgroundMt);

                // if goal then we're done
                if (bestState.distToGoal() == 0)
                {
                    // return with the solutionMt already connected
                    commitSolution(bestState, solutionMt, nowMt, backgroundMt);
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
                    List<string> nextModList = new List<string>();
                    foreach (string m in bestState.modList) nextModList.Add(m);
                    nextModList.Add(nextModule);
                    List<string> nextGoalSet = transformAct(nextModule, bestState.missingList);

                    GoapState nextState = new GoapState(nextModList, nextGoalSet);
                    nextState.totalCost = bestState.totalCost + nextCost;
                    // measure the quality of the next state
                    setSolution(nextState, solutionMt, nowMt, backgroundMt);

                    //skip if it has been examined
                    if (closedSet.Contains(nextState))
                        continue;

                    if (!openSet.Contains(nextState))
                    {
                        openSet.Add(nextState);
                        gScores[nextState.idCode]= nextState.costSoFar();
                        hScores[nextState.idCode]= nextState.distToGoal() * problemWorstCost;
                        fScores[nextState.idCode]=(gScores[nextState.idCode] + hScores[nextState.idCode]);
                    }
                }
                openSet.Sort();
            }
            // an impossible task appently
            commitSolution(start, solutionMt, nowMt, backgroundMt);
            return false;

        }

        public void setSolution(GoapState cState, string solutionMt, string nowMt, string backgroundMt)
        {
            // Make the description in cState the focus
            prologEngine.clearKB(solutionMt);
            prologEngine.clearConnectionsFromMt(solutionMt);
            if (backgroundMt !=null) prologEngine.connectMT(solutionMt, backgroundMt);
           // foreach (string moduleMt in cState.modList)
           // {
           //     prologEngine.connectMT(solutionMt, moduleMt);
           // }
            string goalCode = "";
            foreach (string p in cState.missingList)
            {
                goalCode += String.Format("precond({0}).\n", p);
            }
            prologEngine.appendKB(goalCode, solutionMt);

            List<string> solutionMissingList = missingInMt(solutionMt, nowMt);
            //List<string> solutionViolationList = violationsInMt(problemMt);

            cState.missingList = solutionMissingList;
            //cState.violationList = solutionViolationList;
        }
        public void commitSolution(GoapState cState, string solutionMt, string nowMt, string backgroundMt)
        {
            planNode = cState;
            // Modules/Actions are in reverse order from now to goal so flip them
            cState.modList.Reverse();

            // Make final connections
            if (backgroundMt != null) prologEngine.connectMT(solutionMt, backgroundMt);
            foreach (string moduleMt in cState.modList)
            {
                prologEngine.connectMT(solutionMt, moduleMt);
            }

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

            prologEngine.appendKB(postScript, solutionMt);


            // post the modules used
            string modString = "";
            if (cState.modList.Count > 0)
            {
                foreach (string m in cState.modList)
                {
                    modString += m + " ";
                }
                prologEngine.appendListPredToMt("modlist", modString, solutionMt);
            }
            else
            {
                prologEngine.appendKB("modlist([]).\n", solutionMt);
            }

            string planSequence = "";
            int planCount = 0;
            if (cState.modList.Count > 0)
            {
                foreach (string m in cState.modList)
                {
                    planSequence += String.Format("planraw({0}).\n", m);
                }
                foreach (string m in cState.modList)
                {
                    planSequence += String.Format("planseq({0},{1}).\n", planCount ,m);
                }
                prologEngine.appendKB(planSequence, solutionMt);
            }
            else
            {
                prologEngine.appendKB("planraw(nop).\n planseq(0,nop).\n", solutionMt);
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
            int totalNodes = openSet.Count + closedSet.Count;
            Console.WriteLine("Planning time = {0}", elapsed);
            Console.WriteLine("Planning list = '{0}'", modString);

            Console.WriteLine("Planning tials = '{0}'", trials);
            Console.WriteLine("TotalNodes = {0}", totalNodes);
            if (trials > 0)
            {
                Console.WriteLine("Planning ms/trials = '{0}'", ((double)elapsed / (double)trials));
            }
            if (totalNodes > 0)
            {
                Console.WriteLine("Planning ms/nodes = '{0}'", ((double)elapsed / (double)totalNodes));
            }
            if (elapsed > 0)
            {
                Console.WriteLine("Planning trials/ms = '{0}'", ((double)trials / (double)elapsed));
                Console.WriteLine("Planning nodes/ms = '{0}'", ((double)totalNodes / (double)elapsed));
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
            if (planNode ==null) return "";

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
        /// <param name="randomBest">Given equal value choices return one at random versus the first found</param>
        /// <returns>Lowest cost state</returns>
        private GoapState FindBest(List<GoapState> set, Dictionary<string, double> fScores, bool randomBest)
        {
            GoapState lowestState = null;
            double lowest = double.MaxValue;
            int bestCount = 1;
            //loop through all states in the list
            foreach (GoapState state in set)
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
