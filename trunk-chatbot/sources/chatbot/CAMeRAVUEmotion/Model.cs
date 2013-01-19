using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
#if (COGBOT_LIBOMV || USE_STHREADS || true)
using ThreadPoolUtil;
using ThreadPoolUtil;
using ThreadStart = System.Threading.ThreadStart;
using AutoResetEvent = System.Threading.AutoResetEvent;
using ManualResetEvent = System.Threading.ManualResetEvent;
using TimerCallback = System.Threading.TimerCallback;
using Timer = System.Threading.Timer;
using EventWaitHandle = System.Threading.EventWaitHandle;
using Timeout = System.Threading.Timeout;
using SynchronizationLockException = System.Threading.SynchronizationLockException;
using Interlocked = System.Threading.Interlocked;
using ThreadExceptionEventHandler = System.Threading.ThreadExceptionEventHandler;
using ThreadExceptionEventArgs = System.Threading.ThreadExceptionEventArgs;
using ApartmentState = System.Threading.ApartmentState;
using ThreadPoolUtil;
#else
using System.Threading;
using Thread = System.Threading.Thread;
#endif

namespace CAMeRAVUEmotion
{
    // This code was taken from Silicon Coppelia,
    // developed within the Center for Advanced Media Research Amsterdam 
    // at the VU University Amsterdam (CAMeRA@VU) 
    // and written by Matthijs Aart Pontier and Ghazanfar Farooq Siddiqui. 
    // More information and publications can be found here:
    // http://camera-vu.nl/matthijs/
    // http://www.linkedin.com/profile/view?id=19933074 
    // http://www.few.vu.nl/~mpr210/
    // http://www.few.vu.nl/~mpr210/DissertationMAPontier.pdf
    // http://camera-vu.nl/matthijs/IAT-2009_Coppelia.pdf
    //Hoorn, J.F., Pontier, M.A., & Siddiqui, G.F., (2011).
    //Coppélius’ Concoction: Similarity and Complementarity
    //Among Three Affect-related Agent Models. Cognitive
    //Systems Research Journal, in press.

    /// <summary>
    /// The Model class represents all calculations that are not subject to change between different simulations.
    /// </summary>
    public class Model
    {
        static Random rng = new Random();
        static int nextAgentID = 0;
        static List<Agent> agents = new List<Agent>();
        /// <summary>
        /// Registers an Agent with the Model. This Agent will then be updated.
        /// </summary>
        /// <param name="toAdd"></param>
        /// <returns></returns>
        public static int RegisterAgent(Agent toAdd)
        {
            agents.Add(toAdd);
            return nextAgentID++;
        }
        /// <summary>
        /// Unregisters an Agent with the Model. This Agent will no longer be updated.
        /// </summary>
        /// <param name="toRemove"></param>
        public static void UnregisterAgent(Agent toRemove)
        {
            agents.Remove(toRemove);
        }

        /// <summary>
        /// Returns an Agent instance via their index.
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        public static Agent GetAgentByID(int id)
        {
            foreach (Agent a in agents)
            {
                if (a.ID == id)
                    return a;
            }

            return null;
        }

        internal static bool waitingForInput = false;
        public static bool WaitingForInput
        {
            get
            {
                return waitingForInput;
            }
        }

        internal static int inputAgent = -1;

        static bool shouldStop = false;

        static bool stalled = false;
        /// <summary>
        /// This variable is set to true when the model exhausts its action-reaction chain
        /// </summary>
        public static bool Stalled
        {
            get
            {
                return stalled;
            }
        }

        public static void Unstall()
        {
            stalled = false;
        }

        /// <summary>
        /// Stops the Model's update sequence.
        /// </summary>
        public static void Stop()
        {
            shouldStop = true;
        }

        static AgentAction temp = new AgentAction("temp", 0, 0);

        public static void SelectAction(List<AgentAction> choices, Agent choosingAgent, Agent targetAgent)
        {
            temp.responseList.Clear();
            
            foreach (AgentAction a in choices)
                temp.AddResponse(a.GlobalIndex);

            targetAgent.ManualPerform(temp, choosingAgent);
        }

        /// <summary>
        /// Runs the Model until a HumanAgent has received an action. The Model will enter a waiting state until that is resolved.
        /// Use the HumanAgent's InputRequest delegate to receive this callback.
        /// </summary>
        /// <returns></returns>
        static bool Run()
        {
            //////////////////////////////
            //START ROUGH IMPLEMENTATION//
            //////////////////////////////

            List<float> p_eu_f = new List<float>();
            List<float> n_eu_f = new List<float>();
            int n = 0, p = 0;

            List<float> p_eu_a = new List<float>();
            List<float> n_eu_a = new List<float>();
            int n1 = 0, p1 = 0;

            List<float> mu_a = new List<float>();

            List<float> p_at_ap = new List<float>();
            List<float> n_at_ap = new List<float>();
            List<float> p_at_an = new List<float>();
            List<float> n_at_an = new List<float>();
            float wp1, wp2, wn1, wn2;

            List<float> p_geu_af = new List<float>();
            List<float> n_geu_af = new List<float>();
            int p_geu = 0, n_geu = 0;

            string tempmove = "";
            int ok = 0;
            int cf = 1;
            float max_esat=-9999;
            int action = -1;
            int target = -1;
            float max2;
            int max_response = 0, max_agentid = 0;
            
            List<float> p_likelihood = new List<float>();
            List<float> n_likelihood = new List<float>();
            int p_like = 0, n_like = 0;

            foreach (Agent agent1 in agents)
            {
                if ( !(agent1 is HumanAgent) && agent1.receivedAction != -1 && !waitingForInput )
                {
                    //*****************calculating EU FEATURE***************************************************
                    foreach (int agent2id in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int state = 0; state < Global.StateCount; ++state)
                            {
                                agent1.SetExpectedUtilityFeature(state, agent2id, feature, agent1.GetFeatureBelief(feature, state, agent2id) * agent1.GetAmbition(state));
                            }
                        }
                    }
                    //*****************calculating EU actions (regarding 1 goal?)***************************************************
                    foreach (int response in agent1.possibleResponses)
                    {
                        AgentAction a = Global.GetActionByID(response);

                        if (a == null)
                            throw new Exception("Invalid response ID, or something went wrong with the global actions dictionary");

                        foreach (int agent2 in agent1.perceivedAgents)
                        {
                            for (int state = 0; state < Global.StateCount; ++state)
                            {
                                float asb = agent1.GetActionStateBelief(a.GlobalIndex, state);
                                float val = agent1.GetAmbition(state);
                                agent1.SetExpectedUtilityAction(state, agent2, response, asb * val);
                            }
                        }
                    }
                    //***********************calculating GEU(AGENT, FEATURE, OTHER_AGENT)****************
                    foreach (int agent2id in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            p_eu_f.Clear();
                            n_eu_f.Clear();

                            for (int state = 0; state < Global.StateCount; ++state)
                            {
                                float temp_eu_a = agent1.GetExpectedUtilityFeature(state, agent2id, feature);

                                if (temp_eu_a > 0)
                                {
                                    p_eu_f.Add(temp_eu_a);
                                    p++;
                                }
                                else if (temp_eu_a < 0)
                                {
                                    n_eu_f.Add(temp_eu_a);
                                    n++;
                                }
                            }

                            p_eu_f.Sort(Global.SortAscending);
                            n_eu_f.Sort(Global.SortDescending);

                            float pos_eu = 0, neg_eu = 0;
                            for (int i = 0; i < p_eu_f.Count; ++i)
                            {
                                pos_eu = (pos_eu + p_eu_f[i]) / 2;
                            }
                            for (int i = 0; i < n_eu_f.Count; ++i)
                            {
                                neg_eu = (neg_eu + n_eu_f[i]) / 2;
                            }
                            float wp, wn;
                            if ((p + n) != 0) { wp = agent1.wGEUpf * p / (p + n); } else wp = 0.5f;
                            if ((p + n) != 0) { wn = agent1.wGEUnf * n / (p + n); } else wn = 0.5f;
                            agent1.SetGEUFeature(agent2id, feature, wp * pos_eu + wn * neg_eu);
                            agent1.SetRelFeature(agent2id, feature, Math.Abs(agent1.GetGEUFeature(agent2id, feature)));
                        }
                    }
                    //***********************calculating GEU(AGENT, ACTIONS, OTHER_AGENT)****************
                    foreach (int response in agent1.possibleResponses)
                    {
                        foreach (int agent2id in agent1.perceivedAgents)
                        { 
                            p1 = 0; n1 = 0;

                            p_eu_a.Clear();
                            n_eu_a.Clear();

                            for (int state = 0; state < Global.StateCount; ++state)
                            {
                                float temp_eu_a = agent1.GetExpectedUtilityAction(state, agent2id, response);

                                if (temp_eu_a > 0)
                                {
                                    p_eu_a.Add(temp_eu_a);
                                    p1++;
                                }
                                else if (temp_eu_a < 0)
                                {
                                    n_eu_a.Add(temp_eu_a);
                                    n1++;
                                }
                            }

                            p_eu_a.Sort(Global.SortAscending);
                            n_eu_a.Sort(Global.SortDescending);
                            float pos_eu1 = 0, neg_eu1 = 0;

                            for( int x = 0; x < p_eu_a.Count; ++x )
                            {
                                pos_eu1 = (pos_eu1 + p_eu_a[x]) / 2;
                            }

                            for (int x = 0; x < n_eu_a.Count; ++x)
                            {
                                neg_eu1 = (neg_eu1 + n_eu_a[x]) / 2;
                            }

                            float wp, wn;
                            if ((p1 + n1) != 0) { wp = agent1.wGEUpa * p1 / (p1 + n1); } else wp = 0.5f;
                            if ((p1 + n1) != 0) { wn = agent1.wGEUna * n1 / (p1 + n1); } else wn = 0.5f;

                            agent1.SetGEUAction(agent2id, response, wp * pos_eu1 + wn * neg_eu1);

                            //Note: I moved this here from the next loop, because it obviously did not need a separate loop
                            agent1.SetAT(agent2id, response, agent1.GetGEUAction(agent2id, response));
                        }
                    }

                    //*****************calculating Morality actions (regarding 1 moral principle)***************************************************
                    foreach (int response in agent1.possibleResponses)
                    {
                        AgentAction a = Global.GetActionByID(response);

                        if (a == null)
                            throw new Exception("Invalid response ID, or something went wrong with the global actions dictionary");

                        foreach (int agent2 in agent1.perceivedAgents)
                        {
                            for (int moralprinciple = 0; moralprinciple < Global.MoralPrincipleCount; ++moralprinciple)
                            {
                                float ampb = agent1.GetActionMoralPrincipleBelief(a.GlobalIndex, moralprinciple);
                                float mval = agent1.GetMoralAmbition(moralprinciple);
                                agent1.SetMoralityAction(moralprinciple, agent2, response, ampb * mval);
                                Console.WriteLine("Set Morality Action " + response + " toward agent " + agent2 + " regarding moral principle " + moralprinciple + " to " + ampb*mval);
                            }
                        }
                    }

 //***********************calculating Morality(AGENT, ACTIONS, OTHER_AGENT)****************
                    foreach (int response in agent1.possibleResponses)
                    {
                        float temp_morality = 0;
                        foreach (int agent2id in agent1.perceivedAgents)
                        {                             
                            for (int moralprinciple = 0; moralprinciple < Global.MoralPrincipleCount; ++moralprinciple)
                            {
                                float temp_mu_a = agent1.GetMoralityAction(moralprinciple, agent2id, response);
                                temp_morality += temp_mu_a;
                            }
                            temp_morality = temp_morality / Global.MoralPrincipleCount;

                            agent1.SetGMoralityAction(agent2id, response, temp_morality);
                            Console.WriteLine("Set General Morality Action " + response + " toward agent " + agent2id + " to " + temp_morality);
                        }
                    }

                    ////Calculating General positivity and negativity action tendencies(GPAT[AGENTS] and GNAT[AGENTS])**********************************
                    foreach (int agent2id in agent1.perceivedAgents)
                    {
                        int p2 = 0, p3 = 0, n2 = 0, n3 = 0;

                        p_at_ap.Clear();
                        n_at_ap.Clear();
                        p_at_an.Clear();
                        n_at_an.Clear();

                        foreach (int response in agent1.possibleResponses)
                        {
                            AgentAction a = Global.GetActionByID(response);

                            if (a == null)
                                throw new Exception("Invalid response index, or something went wrong with the actions dictionary");

                            float temp_at = agent1.GetAT(agent2id, response);

                            if (agent1.GetAT(agent2id, response) > 0)
                            {
                                p_at_ap.Add(temp_at * a._positivity);
                                p2++;
                                n_at_ap.Add(temp_at * a._negativity);
                                n2++;
                            }
                            else if (agent1.GetAT(agent2id, response) < 0)
                            {
                                p_at_an.Add(temp_at * a._positivity);
                                p3++;
                                n_at_an.Add(temp_at * a._negativity);
                            }
                        }

                        p_at_ap.Sort(Global.SortAscending);
                        n_at_ap.Sort(Global.SortDescending);
                        p_at_an.Sort(Global.SortAscending);
                        n_at_an.Sort(Global.SortDescending);
                        float pos_atp = 0, neg_atp = 0, pos_atn = 0, neg_atn = 0;

                        for (int y = 0; y < p2; y++)
                            pos_atp = (pos_atp + p_at_ap[y]) * 0.5f;
                        for (int y = n2 - 1; y >= 0; --y)
                            neg_atp = ((neg_atp) + (n_at_ap[y])) * 0.5f;
                        for (int y = 0; y < p3; y++)
                            pos_atn = (pos_atn + p_at_an[y]) * 0.5f;
                        for (int y = n3 - 1; y >= 0; --y)
                            neg_atn = ((neg_atn) + (n_at_an[y])) * 0.5f;

                        if ((p2 + n2) != 0)
                        {
                            wp1 = agent1.wGPATp * p2 / (p2 + n2);
                            wn1 = agent1.wGPATn * n2 / (p2 + n2);
                        }
                        else { wp1 = 0.5f; wn1 = 0.5f; }
                        if ((p3 + n3) != 0)
                        {
                            wp2 = agent1.wGNATp * p3 / (p3 + n3);
                            wn2 = agent1.wGNATn * n3 / (p3 + n3);
                        }
                        else { wp2 = 0.5f; wn2 = 0.5f; }

                        agent1.SetGPAT(agent2id, wp1 * pos_atp + wn1 * neg_atp);
                        agent1.SetGNAT(agent2id, wp2 * pos_atn + wn2 * neg_atn);
                    }
                    ////*********Calculating appraisal variables(similarity, dissimilarity, relevance, irrelevance, pos_valance, neg_valance)***********************************************************	
                    foreach (int agent2id in agent1.perceivedAgents)
                    {
                        Agent agent2 = GetAgentByID(agent2id);

                        agent1.SetRelation(agent2id, AgentRelations.SIMILARITY,
                                                agent1.wsg * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.GOOD) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD)) +
                                                agent1.wsb * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.BAD) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD)) +
                                                agent1.wsbea * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.BEAUTIFUL) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL)) +
                                                agent1.wsu * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.UGLY) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY)) +
                                                agent1.wsr * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.REALISTIC) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.REALISTIC)) +
                                                agent1.wsunr * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.UNREALISTIC) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.UNREALISTIC))
                                            );

                        agent1.SetRelation(agent2id, AgentRelations.DISSIMILARITY,
                                                agent1.wdsg * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.GOOD) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD)) +
                                                agent1.wdsb * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.BAD) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD)) +
                                                agent1.wdsbea * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.BEAUTIFUL) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL)) +
                                                agent1.wdsu * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.UGLY) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY)) +
                                                agent1.wdsr * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.REALISTIC) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.REALISTIC)) +
                                                agent1.wdsunr * Math.Abs(agent1.GetPerceivedFeature(agent1.ID, AgentFeatures.UNREALISTIC) - agent1.GetPerceivedFeature(agent2id, AgentFeatures.UNREALISTIC))
                                            );

                        agent1.SetRelation(agent2id, AgentRelations.RELEVANCE,
                                              agent1.wrg * agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD) +
                                              agent1.wrb * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD) +
                                              agent1.wrpos * Math.Abs((agent1.GetGPAT(agent2id) + 1) * 0.5f) +
                                              agent1.wrneg * Math.Abs((agent1.GetGNAT(agent2id) + 1) * 0.5f)
                                          );

                        agent1.SetRelation(agent2id, AgentRelations.IRRELEVANCE,
                                               agent1.wirrg * agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD) +
                                               agent1.wirrb * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD) +
                                               agent1.wipos * Math.Abs((agent1.GetGPAT(agent2id) + 1) * 0.5f) +
                                               agent1.wineg * Math.Abs((agent1.GetGNAT(agent2id) + 1) * 0.5f)
                                          );

                        agent1.SetRelation(agent2id, AgentRelations.P_VALENCE,
                                                agent1.wpvg * agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD) +
                                                agent1.wpvb * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD) +
                                                agent1.wpvpos * (agent1.GetGPAT(agent2id) + 1) * 0.5f +
                                                agent1.wpvneg * (agent1.GetGNAT(agent2id) + 1) * 0.5f
                                        );

                        agent1.SetRelation(agent2id, AgentRelations.N_VALENCE,
                                                agent1.wnvg * agent1.GetPerceivedFeature(agent2id, AgentFeatures.GOOD) +
                                                agent1.wnvb * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BAD) +
                                                agent1.wnvpos * (agent1.GetGPAT(agent2id) + 1) * 0.5f +
                                                agent1.wnvneg * (agent1.GetGNAT(agent2id) + 1) * 0.5f
                                        );

                        //=========calculating Involvement and distance=========================================	

                        agent1.SetRelation(agent2id, AgentRelations.INVOLVEMENT,
                                                agent1.wibe * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wiu * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wireal * agent1.GetPerceivedFeature(agent2id, AgentFeatures.REALISTIC) +
                                                agent1.wiunr * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UNREALISTIC) +
                                                agent1.wiaid * agent1.GetPerceivedFeature(agent2id, AgentFeatures.AID) +
                                                agent1.wiobstacle * agent1.GetPerceivedFeature(agent2id, AgentFeatures.OBSTACLE) +
                                                agent1.wipv * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) +
                                                agent1.winv * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) +
                                                agent1.wipvs * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.winvs * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wipvd * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.winvd * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wipvbe * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.winvbe * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wipvu * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.winvu * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wir * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) +
                                                agent1.wiirr * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) +
                                                agent1.wirs * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wiirrs * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wird * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wiirrd * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wirbe * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wiirrbe * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wiru * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wiirru * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY)
                                            );

                        agent1.SetRelation(agent2id, AgentRelations.DISTANCE,
                                                agent1.wdbe * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wdu * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wdreal * agent1.GetPerceivedFeature(agent2id, AgentFeatures.REALISTIC) +
                                                agent1.wdunr * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UNREALISTIC) +
                                                agent1.wdaid * agent1.GetPerceivedFeature(agent2id, AgentFeatures.AID) +
                                                agent1.wdobstacle * agent1.GetPerceivedFeature(agent2id, AgentFeatures.OBSTACLE) +
                                                agent1.wdpv * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) +
                                                agent1.wdnv * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) +
                                                agent1.wdpvs * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wdnvs * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wdpvd * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wdnvd * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wdpvbe * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wdnvbe * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wdpvu * agent1.GetRelation(agent2id, AgentRelations.P_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wdnvu * agent1.GetRelation(agent2id, AgentRelations.N_VALENCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wdr * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) +
                                                agent1.wdirr * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) +
                                                agent1.wdrs * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wdirrs * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.SIMILARITY) +
                                                agent1.wdrd * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wdirrd * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetRelation(agent2id, AgentRelations.DISSIMILARITY) +
                                                agent1.wdrbe * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wdirrbe * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.BEAUTIFUL) +
                                                agent1.wdru * agent1.GetRelation(agent2id, AgentRelations.RELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY) +
                                                agent1.wdirru * agent1.GetRelation(agent2id, AgentRelations.IRRELEVANCE) * agent1.GetPerceivedFeature(agent2id, AgentFeatures.UGLY)
                                            );
                    }

                    //*************calculating Use intentions**************
                    //*****************************************************
                    foreach (int agent2id in agent1.perceivedAgents)
                    {
                        p_geu = 0;
                        n_geu = 0;
                        p_geu_af.Clear();
                        n_geu_af.Clear();

                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            float temp_geu_f = agent1.GetGEUFeature(agent2id, feature);

                            if (temp_geu_f > 0)
                            {
                                p_geu_af.Add(temp_geu_f);
                                p_geu++;
                            }
                            else if (temp_geu_f < 0)
                            {
                                n_geu_af.Add(temp_geu_f);
                                n_geu++;
                            }
                        }

                        foreach (int response in agent1.possibleResponses)
                        {
                            float temp_geu_a = agent1.GetGEUAction(agent2id, response);

                            if (temp_geu_a > 0)
                            {
                                p_geu_af.Add(temp_geu_a);
                                p_geu++;
                            }
                            else if (temp_geu_a < 0)
                            {
                                n_geu_af.Add(temp_geu_a);
                                p_geu++;
                            }
                        }

                        p_geu_af.Sort(Global.SortAscending);
                        n_geu_af.Sort(Global.SortDescending);
                        float pos_geu = 0, neg_geu = 0;

                        for (int j = 0; j < p_geu_af.Count; ++j)
                            pos_geu = (pos_geu + p_geu_af[j]) * 0.5f;

                        for (int j = n_geu_af.Count - 1; j >= 0; --j)
                        {
                            neg_geu = (neg_geu + n_geu_af[j]) * 0.5f;
                        }
                        float wp, wn;
                        if ((p_geu + n_geu) != 0) { wp = agent1.wUIp * p_geu / (p_geu + n_geu); } else wp = 0.5f;
                        if ((p_geu + n_geu) != 0) { wn = agent1.wUIn * n_geu / (p_geu + n_geu); } else wn = 0.5f;

                        agent1.SetRelation(agent2id, AgentRelations.USE_INTENTION, wp * pos_geu + wn * neg_geu);

                       
                        //==================================================		
                        //********************************************************************		
                        //**********CALCULATING inv_dis_tradeoff******************************

                        agent1.SetRelation(agent2id, AgentRelations.INV_DIST_TRADEOFF,
                                            agent1.trade_off_gamma * Math.Max(agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT), agent1.GetRelation(agent2id, AgentRelations.DISTANCE)) +
                                            (1 - agent1.trade_off_gamma) * (agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT) + agent1.GetRelation(agent2id, AgentRelations.DISTANCE)) * 0.5f
                                            );

                        //**********CALCULATING expected satisfaction****************************
                        agent1.SetRelation(agent2id, AgentRelations.EXP_SATISFACTION,
                                                agent1.wesidt * agent1.GetRelation(agent2id, AgentRelations.INV_DIST_TRADEOFF) +
                                                agent1.wesui * (agent1.GetRelation(agent2id, AgentRelations.USE_INTENTION) + 1) * 0.5f
                                            );
                    }

                    //**********CALCULATING expected satisfaction ACTIONS******************************
                    foreach (int response in agent1.possibleResponses)
                    {
                        foreach (int agent2id in agent1.perceivedAgents)
                        {
                            AgentAction a = Global.GetActionByID(response);

                            if (a == null)
                                throw new Exception("Invalid response ID, or something went wrong with the actions dictionary");

                            //Wat gebeurt er precies?
                            Console.WriteLine("Calculating Expected Satisfaction. Action " + Global.GetActionByID(response).Name + ", Agent " + agent2id); 
                            Console.WriteLine(" Morality = " + agent1.GetGMoralityAction(agent2id, response));
                            Console.WriteLine(" Expected Utility = " + agent1.GetGEUAction(agent2id, response));
                            Console.WriteLine(" inv = " + (agent1.biasinv * agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT)));  
                            Console.WriteLine(" dis = " + (agent1.biasdis * agent1.GetRelation(agent2id, AgentRelations.DISTANCE)));
                            Console.WriteLine(" 1-d(pos, inv) = " + (1 - Math.Abs(a._positivity - agent1.biasinv * agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT))));                    
                            Console.WriteLine(" 1-d(neg, dis) = " + (1 - Math.Abs(a._negativity - agent1.biasdis * agent1.GetRelation(agent2id, AgentRelations.DISTANCE))));
                            Console.WriteLine(" ES = " + (agent1.wesmor * agent1.GetGMoralityAction(agent2id, response) + 
                                    agent1.wesaeu * agent1.GetGEUAction(agent2id, response) +
                                    agent1.wesapos * (1 - Math.Abs(a._positivity - agent1.biasinv * agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT))) +
                                    agent1.wesaneg * (1 - Math.Abs(a._negativity - agent1.biasdis * agent1.GetRelation(agent2id, AgentRelations.DISTANCE))))
                                );

                            agent1.SetExpectedSatisfaction(agent2id, response,
                                    agent1.wesmor * agent1.GetGMoralityAction(agent2id, response) + 
                                    agent1.wesaeu * agent1.GetGEUAction(agent2id, response) +
                                    agent1.wesapos * (1 - Math.Abs(a._positivity - agent1.biasinv * agent1.GetRelation(agent2id, AgentRelations.INVOLVEMENT))) +
                                    agent1.wesaneg * (1 - Math.Abs(a._negativity - agent1.biasdis * agent1.GetRelation(agent2id, AgentRelations.DISTANCE)))
                                );
                        }
                    }

                    //****************Calculated the maximum expected satisfaction for each agent and performing an Action***************
                    //TODO: FIND OUT IF THIS IS A VALID BASE-NUMBER!!!
                    
                    // Original
                    /*
                    max2 = -1;
                    foreach (int response in agent1.possibleResponses)
                    {
                        foreach (int agent2id in agent1.perceivedAgents)
                        {
                            float temp_esat = agent1.GetExpectedSatisfaction(agent2id, response);

                            if (temp_esat > max2)
                            {
                                max2 = temp_esat;
                                max_response = response;
                                max_agentid = agent2id;
                            }


                        }
                        if (max2 > max_esat)
                        {
                            max_esat = max2;
                            action = max_response;
                            target = max_agentid;
                        }
                    }
                    */

                    // Random selection amongst best options
                    max2 = -1;
                    int actCount1 = 0;
                    foreach (int response in agent1.possibleResponses)
                    {

                        foreach (int agent2id in agent1.perceivedAgents)
                        {
                            float temp_esat = agent1.GetExpectedSatisfaction(agent2id, response);

                            // probablistically choose amongst equals
                            if (temp_esat == max2)
                            {
                                actCount1++;
                                double replaceP = 1 / (double)actCount1;
                                if (rng.NextDouble() < replaceP)
                                {
                                    max_response = response;
                                    max_agentid = agent2id;
                                }
                            }
                            if (temp_esat > max2)
                            {
                                actCount1 = 1;
                                max2 = temp_esat;
                                max_response = response;
                                max_agentid = agent2id;
                            }
                        }
                    }
                    max_esat = max2;
                    action = max_response;
                    target = max_agentid;

                    //*******Queue the selected action for performance after this timestep *************
                    //Console.WriteLine("queueing action: " + action + " towards agent: " + target);
                    agent1.queuedAction = action;
                    agent1.queuedTarget = target;
                    //*************Calculating agent's perception of responsibility of agents for goals
                    //TODO: Figure out if this is indeed supposed to be the agent from which he received the action
                    for (int state = 0; state < Global.StateCount; ++state)
                    {
                        AgentAction a = Global.GetActionByID(agent1.receivedAction);

                        if (agent1.GetActionStateBelief(a.GlobalIndex, state) > 0)
                        {
                            agent1.SetAgentResponsibleBelief(agent1.receivedAgent, state,
                                                                                            agent1.GetAgentResponsibleBelief(agent1.receivedAgent, state) +
                                                                                            agent1.mf_respbel * agent1.GetActionStateBelief(a.GlobalIndex, state) *
                                                                                            (1 - agent1.GetAgentResponsibleBelief(agent1.receivedAgent, state))
                                                             );
                        }
                        else if (agent1.GetActionStateBelief(a.GlobalIndex, state) < 0)
                        {
                            agent1.SetAgentResponsibleBelief(agent1.receivedAgent, state,
                                                                                            agent1.GetAgentResponsibleBelief(agent1.receivedAgent, state) +
                                                                                            agent1.mf_respbel * agent1.GetActionStateBelief(a.GlobalIndex, state) *
                                                                                            (1 + agent1.GetAgentResponsibleBelief(agent1.receivedAgent, state))
                                                            );
                        }

                        //Calculating believed world-state according to actions of received agent
                        if (agent1.GetActionStateBelief(a.GlobalIndex, state) > 0)
                        {
                            agent1.SetStateBelief(state, agent1.GetStateBelief(state) + agent1.mf_agent_bel_state * agent1.GetActionStateBelief(a.GlobalIndex, state) * (1 - agent1.GetStateBelief(state)));
                        }
                        else if (agent1.GetActionStateBelief(a.GlobalIndex, state) < 0)
                        {
                            agent1.SetStateBelief(state, agent1.GetStateBelief(state) + agent1.mf_agent_bel_state * agent1.GetActionStateBelief(a.GlobalIndex, state) * agent1.GetStateBelief(state));
                        }

                        //Calculating state likelihood
                        p_like = n_like = 0;
                        p_likelihood = new List<float>();
                        n_likelihood = new List<float>();
                        for( int state2 = 0; state2 < Global.StateCount; ++state2 )
                        {
                            float stateFacStateBelief = agent1.GetStateFacStateBelief(state, state2);
                            if (stateFacStateBelief > 0)
                            {
                                p_likelihood.Add(stateFacStateBelief * agent1.GetStateBelief(state));
                                p_like++;
                            }
                            else if (stateFacStateBelief < 0)
                            {
                                n_likelihood.Add(stateFacStateBelief * agent1.GetStateBelief(state));
                                n_like++;
                            }

                            p_likelihood.Sort(Global.SortAscending);
                            n_likelihood.Sort(Global.SortDescending);
                            float pos_like = 0, neg_like = 0;

                            for (int i = 0; i < p_like; ++i)
                                pos_like = (pos_like + p_likelihood[i] / 2);
                            for (int i = 0; i < n_like; ++i)
                                neg_like = ((neg_like) + (n_likelihood[i])) / 2;

                            float wp, wn;
                            if ((p_like + n_like) != 0) { wp = agent1.w_likelihood_p * p_like / (p_like + n_like); } else wp = 0.5f;
                            if ((p_like + n_like) != 0) { wn = agent1.w_likelihood_n * n_like / (p_like + n_like); } else wn = 0.5f;
                            //agent_bel_likelihood_goal[i][k] = wp * pos_like + wn * neg_like;
                            agent1.SetStateLikelihood( state2, wp * pos_like + wn * neg_like );
                        }

                        //update state likelihood based on receivedAction
                        if (agent1.GetActionStateBelief(a.GlobalIndex, state) > 0)
                        {
                            //agent_bel_likelihood_goal[i][s] = agent_bel_likelihood_goal[i][s] + mf_agent_bel_state * HC[j][s + 4] * (1 - agent_bel_likelihood_goal[i][s]); 
                            agent1.SetStateLikelihood(state, agent1.GetStateLikelihood(state) + agent1.mf_agent_bel_state * agent1.GetActionStateBelief(a.GlobalIndex, state) * (1 - agent1.GetStateLikelihood(state)));
                        }
                        else if (agent1.GetActionStateBelief(a.GlobalIndex, state) < 0)
                        {
                            //agent_bel_likelihood_goal[i][s] = agent_bel_likelihood_goal[i][s] + mf_agent_bel_state * HC[j][s + 4] * agent_bel_likelihood_goal[i][s];
                            agent1.SetStateLikelihood(state, agent1.GetStateLikelihood(state) + agent1.mf_agent_bel_state * agent1.GetActionStateBelief(a.GlobalIndex, state) * agent1.GetStateLikelihood(state));
                        }

                        ////*************Calculating agent's perception of responsibility self for goals
                        float actionInfluence = agent1.GetActionStateBelief(action, state);
                        if (actionInfluence > 0)
                        {
                            agent1.SetAgentResponsibleBelief(agent1.ID, state, agent1.GetAgentResponsibleBelief(agent1.ID, state) + agent1.mf_respbel * actionInfluence * (1 - agent1.GetAgentResponsibleBelief(agent1.ID, state))); 
                        }
                        else if (actionInfluence > 0)
                        {
                            agent1.SetAgentResponsibleBelief(agent1.ID, state, agent1.GetAgentResponsibleBelief(agent1.ID, state) + agent1.mf_respbel * actionInfluence * (1 + agent1.GetAgentResponsibleBelief(agent1.ID, state))); 
                        }

                        //******Agents have belief about whether world states are true or false**************************************************************
                        //******If an agent observes something, it believes it is true**********/
                        if (actionInfluence > 0)
                        {
                            agent1.SetStateBelief(state, agent1.GetStateBelief(state) + agent1.mf_agent_bel_state_AC * actionInfluence * (1 - agent1.GetStateBelief(state)));
                            agent1.SetStateLikelihood(state, agent1.GetStateLikelihood(state) + agent1.mf_agent_bel_state_AC * actionInfluence * (1 - agent1.GetStateLikelihood(state)));
                        }
                        else if (actionInfluence < 0)
                        {
                            agent1.SetStateBelief(state, agent1.GetStateBelief(state) + agent1.mf_agent_bel_state_AC * actionInfluence * agent1.GetStateBelief(state));
                            agent1.SetStateLikelihood(state, agent1.GetStateLikelihood(state) + agent1.mf_agent_bel_state_AC * actionInfluence * agent1.GetStateLikelihood(state));
                        }

                        if (agent1.GetAmbition(state) != 0)
                        {
                            foreach (int a2 in agent1.perceivedAgents)
                            {
                                Agent agent2 = GetAgentByID(a2);
                                agent1.SetPraiseworthy(agent2.ID, agent1.GetPraiseworthy(agent2.ID) + agent1.mf_blame * agent1.GetAgentResponsibleBelief(agent2.ID, state) * agent1.GetStateBelief(state) * agent1.GetAmbition(state) * ( ( agent1.GetAmbition(state) > 0 ? 1 : -1 ) - agent1.GetPraiseworthy(agent2.ID)));
                            }
                        }

                        //TODO: Emotions
                        List<float> p_hope_for_goal = new List<float>();
                        List<float> n_hope_for_goal = new List<float>();

                        int p_hope_g = 0, n_hope_g = 0;

                        if ((agent1.fatalism >= 0.5f) && (-0.25f * (Math.Cos(1 / agent1.fatalism * Math.PI * 0.5f) - 1.5f) * agent1.GetAmbition(state) > 0))
                        {
                            p_hope_for_goal.Add((float)(-0.25f * (Math.Cos(1 / agent1.fatalism * Math.PI * agent1.GetStateLikelihood(state)) - 1.5f) * agent1.GetAmbition(state)));
                        }
                        if ((agent1.fatalism >= 0.5f) && (-0.25f * (Math.Cos(1 / agent1.fatalism * Math.PI * 0.5f) - 1.5f) * agent1.GetAmbition(state) < 0))
                        {
                            n_hope_for_goal.Add((float)(-0.25f * (Math.Cos(1 / agent1.fatalism * Math.PI * agent1.GetStateLikelihood(state)) - 1.5f) * agent1.GetAmbition(state)));
                            //alert("likelihood = "+agent_bel_likelihood_goal[i][j]);
                            //alert("ambition = "+agent_ambition[i][j]);
                            //alert("rule2b hope_for_goal "+j+" = "+n_hope_for_goal[n_hope_g]);
                            n_hope_g++;
                        }

                        if ((agent1.fatalism < 0.5f) && ((-0.25f * (Math.Cos(1 / (1 - agent1.fatalism) * Math.PI * (1 - 0.5f)) - 1.5) * agent1.GetAmbition(state)) > 0))
                        {
                            p_hope_for_goal.Add( (float)( -0.25f * (Math.Cos(1 / (1 - agent1.fatalism) * Math.PI * (1 - agent1.GetStateLikelihood(state))) - 1.5f) * agent1.GetAmbition(state) ) );
                            //alert("likelihood = "+agent_bel_likelihood_goal[i][j]);
                            //alert("ambition = "+agent_ambition[i][j]);
                            //alert("rule3b hope_for_goal "+j+" = "+p_hope_for_goal[p_hope_g]);
                            p_hope_g++;
                        }

                        if ((agent1.fatalism < 0.5f) && ((-0.25f * (Math.Cos(1 / (1 - agent1.fatalism) * Math.PI * (1 - 0.5f)) - 1.5f) * agent1.GetAmbition(state)) < 0))
                        {
                            n_hope_for_goal.Add( (float)( -0.25f * (Math.Cos(1 / (1 - agent1.fatalism) * Math.PI * (1 - agent1.GetStateLikelihood(state))) - 1.5f) * agent1.GetAmbition(state) ) );
                            //alert("likelihood = "+agent_bel_likelihood_goal[i][j]);
                            //alert("ambition = "+agent_ambition[i][j]);
                            //alert("rule4b hope_for_goal "+j+" = "+n_hope_for_goal[n_hope_g]);
                            n_hope_g++;
                        }

                        p_hope_for_goal.Sort(Global.SortAscending);
                        n_hope_for_goal.Sort(Global.SortDescending);
                        float pos_hope = 0, neg_hope = 0;

                        for (int k = 0; k < p_hope_g; k++)
                            pos_hope = (pos_hope + p_hope_for_goal[k]) / 2;
                        for (int k = n_hope_g - 1; k >= 0; --k)
                            neg_hope = ((neg_hope) + (n_hope_for_goal[k])) / 2;

                        agent1.SetEmotion(AgentEmotions.HOPE, pos_hope);
                        agent1.SetEmotion(AgentEmotions.FEAR, Math.Abs(neg_hope));

                        //*****************Calculating Joy and Distress
                        if ((agent1.GetAmbition(state) * (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state))) > 0)
                        {
                            agent1.SetEmotion(AgentEmotions.JOY, agent1.GetEmotion(AgentEmotions.JOY) + agent1.mf_joy * agent1.GetAmbition(state) * (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state)) * (1 - agent1.GetEmotion(AgentEmotions.JOY)));
                            agent1.SetEmotion(AgentEmotions.DISTRESS, agent1.GetEmotion(AgentEmotions.DISTRESS) + agent1.mf_distress * (-agent1.GetAmbition(state)) * (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state)) * agent1.GetEmotion(AgentEmotions.DISTRESS) );
                        }

                        if ((agent1.GetAmbition(state) *  (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state))) < 0)
                        {
                           agent1.SetEmotion(AgentEmotions.JOY, agent1.GetEmotion(AgentEmotions.JOY) + agent1.mf_joy * agent1.GetAmbition(state) * (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state)) * agent1.GetEmotion(AgentEmotions.JOY) );
                           agent1.SetEmotion(AgentEmotions.DISTRESS, agent1.GetEmotion(AgentEmotions.DISTRESS) + agent1.mf_distress * (- agent1.GetAmbition(state)) * (agent1.GetStateBelief(state) - agent1.GetStateBelieved(state)) * (1 - agent1.GetEmotion(AgentEmotions.DISTRESS)) );
                        }

                        //*************Calculating anger and guilt*************************		
                        foreach (int a2 in agent1.perceivedAgents)
                        {
                            Agent agent2 = GetAgentByID(a2);
                            //anger
                            if (-(agent1.GetAgentResponsibleBelief(agent2.ID, state) * agent1.GetAmbition(state)) > 0)
                            {
                                agent1.SetAnger( agent2.ID, agent1.GetAnger( agent2.ID ) + agent1.mf_anger * -agent1.GetAgentResponsibleBelief(agent2.ID, state ) * agent1.GetAmbition(state) * (1 - agent1.GetAnger(agent2.ID)) );
                                agent1.SetDesignedBias( agent2.ID, AgentFeatures.GOOD,  agent1.GetDesignedBias( agent2.ID, AgentFeatures.GOOD ) - agent1.mf_bias_ethics * agent1.GetAmbition(state) * agent1.GetDesignedBias(agent2.ID, AgentFeatures.GOOD ) );
                                agent1.SetDesignedBias( agent2.ID, AgentFeatures.BAD, agent1.GetDesignedBias( agent2.ID, AgentFeatures.BAD ) + agent1.mf_bias_ethics * agent1.GetAmbition(state) * (2 - agent1.GetDesignedBias(agent2.ID, AgentFeatures.BAD )) );
                            }
                            if ((-agent1.GetAgentResponsibleBelief(agent2.ID, state) * agent1.GetAmbition(state)) < 0)
                            {
                                agent1.SetAnger( agent2.ID, agent1.GetAnger( agent2.ID ) + agent1.mf_anger * -agent1.GetAgentResponsibleBelief(agent2.ID, state ) * agent1.GetAmbition(state) * agent1.GetAnger(agent2.ID) );
                                agent1.SetDesignedBias( agent2.ID, AgentFeatures.GOOD,  agent1.GetDesignedBias( agent2.ID, AgentFeatures.GOOD ) + agent1.mf_bias_ethics * -agent1.GetAmbition(state) * agent1.GetDesignedBias(agent2.ID, AgentFeatures.GOOD ) );
                                agent1.SetDesignedBias( agent2.ID, AgentFeatures.BAD, agent1.GetDesignedBias( agent2.ID, AgentFeatures.BAD ) - agent1.mf_bias_ethics * -agent1.GetAmbition(state) * (2 - agent1.GetDesignedBias(agent2.ID, AgentFeatures.BAD )) );
                            }
                            
                             //anger_at[0][1] = decay_anger * anger_at[0][1];
                            //emotion[0][anger] = anger_at[0][1];
                            agent1.SetAnger( agent2.ID, agent1.decay_anger * agent1.GetAnger( agent2.ID ) );
                            agent1.SetEmotion( AgentEmotions.ANGER, agent1.GetAnger( agent2.ID ) );
                        }

                        //guilt
                        if ((-agent1.GetAgentResponsibleBelief(agent1.ID, state) * agent1.GetAmbition(state)) > 0)
                        {
                            agent1.SetAnger( agent1.ID, agent1.GetAnger( agent1.ID ) + agent1.mf_anger * -agent1.GetAgentResponsibleBelief(agent1.ID, state) * agent1.GetAmbition(state) * (1 - agent1.GetAnger(agent1.ID)) );
                            agent1.SetDesignedBias( agent1.ID, AgentFeatures.GOOD, agent1.GetDesignedBias( agent1.ID, AgentFeatures.GOOD ) - agent1.mf_bias_ethics * agent1.GetAmbition(state) * agent1.GetDesignedBias( agent1.ID, AgentFeatures.GOOD ) );
                            agent1.SetDesignedBias( agent1.ID, AgentFeatures.BAD, agent1.GetDesignedBias( agent1.ID, AgentFeatures.BAD ) + agent1.mf_bias_ethics * agent1.GetAmbition(state) * (2 - agent1.GetDesignedBias( agent1.ID, AgentFeatures.BAD )) );
                        }
                        if ((-agent1.GetAgentResponsibleBelief(agent1.ID, state ) * agent1.GetAmbition(state)) < 0)
                        {
                            agent1.SetAnger( agent1.ID, agent1.GetAnger( agent1.ID ) + agent1.mf_anger * -agent1.GetAgentResponsibleBelief( agent1.ID, state ) * agent1.GetAmbition(state) * agent1.GetAnger( agent1.ID ) );
                            //anger_at[0][0] = anger_at[0][0] + mf_anger * -agent_bel_agent_responsible[0][0][s] * agent_ambition[0][s] * anger_at[0][0];
                            agent1.SetDesignedBias( agent1.ID, AgentFeatures.GOOD, agent1.GetDesignedBias( agent1.ID, AgentFeatures.GOOD ) + agent1.mf_bias_ethics * -agent1.GetAmbition(state) * agent1.GetDesignedBias(agent1.ID, AgentFeatures.GOOD ) );                                
                            //bias_designed[0][0][0] = bias_designed[0][0][0] + mf_bias_ethics * -agent_ambition[0][s] * bias_designed[0][0][0];
                            agent1.SetDesignedBias( agent1.ID, AgentFeatures.BAD, agent1.GetDesignedBias( agent1.ID, AgentFeatures.BAD ) - agent1.mf_bias_ethics * -agent1.GetAmbition(state) * (2 - agent1.GetDesignedBias(agent1.ID, AgentFeatures.BAD ) ) );
                            //bias_designed[0][0][1] = bias_designed[0][0][1] - mf_bias_ethics * -agent_ambition[0][s] * (2 - bias_designed[0][0][1]);
                        }

                        //anger_at[0][0] = decay_anger * anger_at[0][0];
                        //emotion[0][guilt] = anger_at[0][0];
                        agent1.SetAnger( agent1.ID, agent1.decay_anger * agent1.GetAnger( agent1.ID ) );
                        agent1.SetEmotion( AgentEmotions.GUILT, agent1.GetAnger( agent1.ID ) );

                        //********************************************************************************************
				        // *******         Attentional deployment based on Relevance Feature            **************
				        //********************************************************************************************
                        foreach( int a2 in agent1.perceivedAgents )
                        {
                            Agent agent2 = GetAgentByID(a2);

                            for( int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature )
                                agent1.SetAttention(agent2.ID, feature, agent1.p_att * agent1.GetAttention(agent2.ID, feature) + ( 1 - agent1.p_att ) * agent1.GetRelFeature(agent2.ID, feature ) );
                        }

                        //********************************************************************************************
				        // ********************         Cognitive Change based on Guilt       ************************
				        //********************************************************************************************
				        //If an agent blames itself (in general), and believes he performed an action that inhibits a desired
				        //Or facilitates an undesired goal-state, he will decrease the belief that the action influences the goal-state
                        
                        if (agent1.GetPraiseworthy(agent1.ID) < agent1.thr_guilt && agent1.GetActionStateBelief(a.GlobalIndex, state) * agent1.GetAmbition(state) > 0)
                        {
                            agent1.SetActionStateBelief(a.GlobalIndex, state, agent1.alpha_guilt_bc * agent1.GetActionStateBelief(a.GlobalIndex, state));
                        }
                    }

                    //******************************************
                    //********************************************************************************************
                    // ***         Beliefs that features causes emotions change according to experiences      ****
                    //********************************************************************************************

                    //QUESTION: Why are these two split?
                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                if (agent1.GetAttention(a2, feature) * (agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) >= 0)
                                {
                                    agent1.SetFeatureEmotionBelief(feature, emotion, agent1.GetFeatureEmotionBelief(feature, emotion) + agent1.alpha_bf * ((agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) * agent1.GetAttention(a2, feature)) * ((1 - agent1.GetFeatureEmotionBelief(feature, emotion)) * 0.5f));
                                }
                            }
                        }
                    }

                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                if (agent1.GetAttention(a2, feature) * (agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) < 0)
                                {
                                    agent1.SetFeatureEmotionBelief(feature, emotion, agent1.GetFeatureEmotionBelief(feature, emotion) + agent1.alpha_bf * ((agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) * agent1.GetAttention(a2, feature)) * ((1 + agent1.GetFeatureEmotionBelief(feature, emotion)) * 0.5f));
                                }
                            }
                        }
                    }

                    //********************************************************************************************
                    // ***         Attention level for features change according to experiences				  ****
                    //********************************************************************************************
                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                agent1.SetAttention(a2, feature, agent1.GetAttention(a2, feature) - (agent1.GetFeatureEmotionBelief(feature, emotion) * (agent1.GetEmotion(emotion) - agent1.GetDesired(emotion))));
                            }
                        }
                    }

                     //******************************************
                    //********************************************************************************************
                    // ***         Beliefs that features causes emotions change according to experiences      ****
                    //********************************************************************************************

                    //QUESTION: Why are these two split?
                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                if (agent1.GetAttention(a2, feature) * (agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) >= 0)
                                {
                                    agent1.SetFeatureEmotionBelief(feature, emotion, agent1.GetFeatureEmotionBelief(feature, emotion) + agent1.alpha_bf * ((agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) * agent1.GetAttention(a2, feature)) * ((1 - agent1.GetFeatureEmotionBelief(feature, emotion)) * 0.5f));
                                }
                            }
                        }
                    }

                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                if (agent1.GetAttention(a2, feature) * (agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) < 0)
                                {
                                    agent1.SetFeatureEmotionBelief(feature, emotion, agent1.GetFeatureEmotionBelief(feature, emotion) + agent1.alpha_bf * ((agent1.GetEmotion(emotion) - agent1.GetOldEmotion(emotion)) * agent1.GetAttention(a2, feature)) * ((1 + agent1.GetFeatureEmotionBelief(feature, emotion)) * 0.5f));
                                }
                            }
                        }
                    }

                    //********************************************************************************************
                    // ***         Attention level for features change according to experiences				  ****
                    //********************************************************************************************
                    foreach (int a2 in agent1.perceivedAgents)
                    {
                        for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                        {
                            for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                            {
                                agent1.SetAttention(a2, feature, agent1.GetAttention(a2, feature) - (agent1.GetFeatureEmotionBelief(feature, emotion) * (agent1.GetEmotion(emotion) - agent1.GetDesired(emotion))));
                            }
                        }
                    }

                    //normalize agent's attention values

                    float sum_attention = 0;

                    for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                    {
                        foreach (int a2 in agent1.perceivedAgents)
                        {
                            sum_attention += agent1.GetAttention(a2, feature);
                        }
                    }
                    for (int feature = 0; feature < AgentFeatures.NUM_VALUES; ++feature)
                    {
                        foreach (int a2 in agent1.perceivedAgents)
                        {
                            agent1.SetAttention(a2, feature, agent1.GetAttention( a2, feature ) / sum_attention );
                        }
                    }

                    agent1.mood = (
								agent1.beta_hope*Math.Abs(agent1.GetEmotion(AgentEmotions.HOPE)-agent1.GetDesired(AgentEmotions.HOPE))+
								agent1.beta_fear*Math.Abs(agent1.GetEmotion(AgentEmotions.FEAR)-agent1.GetDesired(AgentEmotions.FEAR))+
								agent1.beta_joy*Math.Abs(agent1.GetEmotion(AgentEmotions.JOY)-agent1.GetDesired(AgentEmotions.JOY))+
								agent1.beta_distress*Math.Abs(agent1.GetEmotion(AgentEmotions.DISTRESS)-agent1.GetDesired(AgentEmotions.DISTRESS))+
								agent1.beta_surprise*Math.Abs(agent1.GetEmotion(AgentEmotions.SURPRISE)-agent1.GetDesired(AgentEmotions.SURPRISE))+
								agent1.beta_anger*Math.Abs(agent1.GetEmotion(AgentEmotions.ANGER)-agent1.GetDesired(AgentEmotions.ANGER))+
								agent1.beta_guilt*Math.Abs(agent1.GetEmotion(AgentEmotions.GUILT)-agent1.GetDesired(AgentEmotions.GUILT))
							   );

                    for (int emotion = 0; emotion < AgentEmotions.NUM_VALUES; ++emotion)
                        agent1.OLD_EMOTIONS[emotion] = agent1.EMOTIONS[emotion];

                    for (int state = 0; state < Global.StateCount; ++state)
                        agent1.SetStateBelieved(state, agent1.GetStateBelief(state));

                    agent1.receivedAction = agent1.receivedAgent = -1;
                }
                else if (agent1 is HumanAgent)
                {
                    if (waitingForInput)
                {
                    //can humans respond even though they are have not received an action?
                        if (agent1.ID == inputAgent)
                    {
                        if ((agent1 as HumanAgent).input != null)
                        {
                                //run a thread and keep running it until it returns good input
                                if (!runningThread)
                                {
                                    pollingAgent = agent1 as HumanAgent;

                                    threadResponse = -1;

                                    Thread T1 = new Thread(GetResponse);
                                    T1.Start();

                                    runningThread = true;
                                }
                                else
                                {
                                    if (threadResponse != -1)
                                    {
                                        //int responseID = (agent1 as HumanAgent).input();
                                        if (threadResponse >= 0)
                                        {
                                            agent1.queuedAction = threadResponse;
                                            agent1.queuedTarget = agent1.receivedAgent;
                                        }

                                        runningThread = false;

                            waitingForInput = false;
                        }
                    }
                                //else
                                //{
                                //    pollingAgent = agent1 as HumanAgent;

                                //    threadResponse = -1;

                                //    System.Threading.Thread T1 = new System.Threading.Thread(GetResponse);
                                //    T1.Start();

                                //    runningThread = true;
                                //}
                            }
                        }
                    }
                    else
                    {
                        //check if we can perform the action that has been queued for our agent
                        if (agent1.queuedAction != -1)
                        {
                            inputAgent = agent1.ID;
                            agent1.Perform();
                        }
                    }
                }
                else
                    continue;
            }

            ////////////////////////////
            //END ROUGH IMPLEMENTATION//
            ////////////////////////////

            stalled = false;// !waitingForInput; //never stall when waiting for input

            foreach (Agent a in agents)
                if (a.Perform())
                    stalled = false;

            if (shouldStop)
                return false;

            return true;
        }

        static bool runningThread = false;
        static HumanAgent pollingAgent = null;
        static int threadResponse = -1;

        static void GetResponse()
        {
            while (threadResponse == -1)
            {
                threadResponse = GetInput();
            }
        }

        static int GetInput()
        {
            return pollingAgent.input();
        }

        /// <summary>
        /// Starts a new thread which loops the Run() function until stopped.
        /// </summary>
        public static void Start()
        {
            Thread T1 = new Thread(RunModel);
            T1.Start();
        }

        static void RunModel()
        {
            while (Run());
        }

        public static void Step()
        {
            Run();
        }
    }
}