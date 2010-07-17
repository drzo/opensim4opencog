using System;
using System.Runtime;
using System.Text;
using System.Xml;
using System.Collections;
using System.Collections.Generic;
using System.IO;
//using System.Linq;
using System.Text.RegularExpressions;
using System.Diagnostics.CodeAnalysis;
using System.Diagnostics;
using RTParser;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    public class process_msm : RTParser.Utils.AIMLTagHandler
    {

        public process_msm(RTParser.RTPBot bot,
                RTParser.User user,
                RTParser.Utils.SubQuery query,
                RTParser.Request request,
                RTParser.Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "processmsm")
            {
                string machine = GetAttribValue("name", null);
                string line = templateNodeInnerText.ToValue(query);
                RTPBot.writeDebugLine("\n\n >>>>>>>>>>>>>>>>>>>>>> PROCESSMSM : |{0}|<<<<<<<<<<<<<<<<<<<<", line);
                //this.user.bot.pMSM.lastDefMachine = machine;
                this.user.bot.pMSM.addMachine(machine);
                // set topic to "collectevidencepatterns"
                //this.user.bot.AddAiml("<set name='topic'>collectevidencepatters</set>");
                //this.user.Predicates.updateSetting("topic", "collectevidencepatters");
                this.user.TopicSetting = "CEP";
                
                // Clear the evidence and next state
                this.user.bot.pMSM.clearEvidence();
                this.user.bot.pMSM.clearNextStateValues();

                // estimate what evidence can be gleaned from the current state
                this.user.bot.pMSM.inspectEvidenceStates();

                // process the input text

                //string evidenceReply = this.user.bot.ChatString(line, this.user.UserID);
                string evidenceReply = subChat(line, this.user.TopicSetting, request,true);

                RTPBot.writeDebugLine("MSM: WithEvidence {0} ", this.user.bot.pMSM.ToString());

                // TODO: we should also get evidence across machine boundries

                // compute the "TRUE" topics from the state machines,advance all machines using evidence
                Hashtable machinesTopState = new Hashtable();
                foreach (string mac in this.user.bot.pMSM.machines.Keys)
                {
                    string top_State = this.user.bot.pMSM.advanceMachine(mac);
                    machinesTopState[mac] = top_State;
                }
                // make the next_state values the new current state values
                this.user.bot.pMSM.advanceStateValues();
                RTPBot.writeDebugLine("MSM: AfterAdvance {0} ", this.user.bot.pMSM.ToString());

                // For each machine set the appropriate topic, and process the input text
                string totalReply = "";
                double strongestVal = -9999;
                foreach (string actingMachine in machinesTopState.Keys)
                {
                    string actionState = (string) machinesTopState[actingMachine];
                    double actp = (double) this.user.bot.pMSM.cur_machineStateVal[actionState];

                    Hashtable topicHt = (Hashtable)this.user.bot.pMSM.machineStateResponses[actionState];

                    if ((topicHt != null) && (topicHt.Count >0))
                    {
                        foreach (string responseTopic in topicHt.Keys )
                        {
                            //prob = topicHt[responseTopic] = prob;
                            string responseTopicUp = responseTopic.ToUpper();
                            //string topicSet = "<set name='topic'>"+responseTopic+"</set>";
                            //this.user.Predicates.updateSetting("topic", responseTopic);
                            this.user.TopicSetting = responseTopicUp;
                            //this.user.bot.AddAiml(topicSet);
                            string actionReply = "";
                            //actionReply = this.user.bot.ChatString(line, this.user.UserID);


                            actionReply = subChat(line, responseTopicUp, request,false);
                            
                            // Append or use non-null response with the most certainty
                            //totalReply += " " + actionReply;
                            RTPBot.writeDebugLine("MSM:[[ PossReply m:{0} s:{1} p:{2} RT: {3} REPLY:{4} ]]  <<< * * * * * *", actingMachine, actionState,actp,responseTopicUp, actionReply.Trim());

                            if ((actp > strongestVal) && (actionReply.Trim().Length >0))
                            {
                                strongestVal = actp;
                                totalReply = actionReply;
                            }
                        }
                    }
                }
                Unifiable result = totalReply.Trim();
                RTPBot.writeDebugLine("MSM: FinalReply :{0}",totalReply.Trim());
                return result;
            }
            return Unifiable.Empty;

        }

        public string subChat(string line, string topic, RTParser.Request request,bool allowMultiplesTemplates)
        {
            //----------------------
            // snarf from "srai"
            Unifiable tempTopic = topic;
            Request subRequest = new AIMLbot.Request(line, this.user, this.Proc, (AIMLbot.Request)request);
            //String gn = GetAttribValue("graph", null);
            string gn = "msm";
            if (gn != null) subRequest.Graph = Proc.GetGraph(gn, request.Graph);
            subRequest.depth = request.depth + 1;

            subRequest.Topic = tempTopic;
            subRequest.ParentRequest = this.request;
            subRequest.StartedOn = this.request.StartedOn;
            subRequest.TimesOutAt = this.request.TimesOutAt;
            subRequest.ProcessMultipleTemplates = allowMultiplesTemplates;

            AIMLbot.Result subResult = null;
            var prev = this.Proc.isAcceptingUserInput;
            var prevSO = user.SuspendAdd;
            try
            {
                this.Proc.isAcceptingUserInput = true;
                var newresult = request.CreateResult(subRequest);
                subRequest.CurrentResult = newresult;
                user.SuspendAdd = true;
                subResult = this.Proc.Chat(subRequest, subRequest.Graph);
               // subResult = this.Proc.Chat(subRequest, request.Graph);
            }
            finally
            {
                user.SuspendAdd = prevSO;
                this.Proc.isAcceptingUserInput = prev;
            }
            //this.request.hasTimedOut = subRequest.hasTimedOut;
            //----------------------
            return subResult.RawOutput.Trim();
        }
    }

    public class actMSM
    {
        // emission and transition stored as double hash tables
        public Hashtable machines = new Hashtable(); // just the machines
        public Hashtable machineStates = new Hashtable(); // [machine]->hashtable[states]
        public Hashtable emissionProbability = new Hashtable(); // evidence -> hashtable[machine:srcState] =prob
        public Hashtable evidenceStateProbability = new Hashtable(); // evidence -> hashtable[machine:srcState] =prob

        public Hashtable transitionCounts = new Hashtable(); // machine:srcState
        public Hashtable stateToEvidence = new Hashtable(); // [machine:state] -> evidence, prob
        public Hashtable atomicTransitionCount = new Hashtable(); // ["machine:srcState machine:dstState"]=count
        public Hashtable currentEvidence = new Hashtable(); // just the current evidence [evidence]=prob

        public Hashtable machineStateResponses = new Hashtable(); // [machine:state] -> response,prob
        public Hashtable machineResponses = new Hashtable(); // just list of machine-> possibe_responses

        public Hashtable cur_machineStateVal = new Hashtable();
        public Hashtable next_machineStateVal = new Hashtable();

        public Hashtable actionStates = new Hashtable();

        //public bot evidenceBot = new bot();
        //public bot actionBot = new bot();

        public double transSum = 0.0001;
        public string lastDefMachine = "m1";
        public string lastDefState="s1";
        public string lastDefEvidence = "e1";

        static actMSM()
        {
        }

        public string ToString()
        {
            string s="";
            foreach (string m in machines.Keys )
            {
                s += "machine:" + m+"\n";
             }
            ArrayList a = new ArrayList();
            foreach (string ev in currentEvidence.Keys){ a.Add(ev);}  a.Sort();
            foreach(string ev in a)
            //foreach (string ev in currentEvidence.Keys)
            {
                s += " evidence " + ev + "=" + (double)currentEvidence[ev] + "\n";
            }
            a.Clear();
            foreach (string st in cur_machineStateVal.Keys) { a.Add(st); } a.Sort();
            foreach (string st in a)
             //foreach (string st in cur_machineStateVal.Keys)
            {
                s += " cur_val " + st + "=" + (double)cur_machineStateVal[st] + "\n";
            }

            a.Clear();
            foreach (string st in next_machineStateVal.Keys) { a.Add(st); } a.Sort();
            foreach (string st in a)
            // foreach (string st in next_machineStateVal.Keys)
            {
                s += " next_val " + st + "=" + (double)next_machineStateVal[st] + "\n";
            }
            return s;
        }
        public void addMachine(string machine)
        {
            RTPBot.writeDebugLine("MSM: addMachine({0})", machine);
            machines[machine] = 1;
            lastDefMachine = machine;
        }

        public void addMachineState(string machine, string state)
        {
                if (!machineStates.ContainsKey(machine))
                {
                    Hashtable h = new Hashtable ();
                    machineStates[machine]=h;
                    
                }
                ((Hashtable) machineStates [machine])[state]=1;
        }
        public void setState(string machine, string state, double prob)
        {
            RTPBot.writeDebugLine("MSM: setState({0},{1},{2})", machine, state, prob);
            addMachineState(machine, state);
            string machineState = machine + ":" + state;
             cur_machineStateVal[machineState] = prob;
        }
        public void defState(string machine, string state, double initProb, double selfProb)
        {
            RTPBot.writeDebugLine("MSM:   defState({0},{1},{2},{3})", machine, state, initProb, selfProb);
            machines[machine] = 1;
            lastDefState = state;
            string machineState = machine + ":" + state;
            addMachineState(machine, state);

            setState(machine, state, initProb);
            addTransition(machine, state, state, selfProb);

        }
        public void clearEvidence()
        {
            RTPBot.writeDebugLine("MSM:  clearEvidence()");
            currentEvidence.Clear();
        }
        public void setEvidence(string evidence, double evidenceProb)
        {
            RTPBot.writeDebugLine("MSM:  setEvidence({0},{1})", evidence, evidenceProb);
            currentEvidence.Add(evidence, evidenceProb);
        }

        public void addResponse(string machine, string myState,string myTopic, double prob)
        {
            addMachineState(machine, myState);

            RTPBot.writeDebugLine("MSM: addResponse({0},{1},{2},{3})", machine, myState, myTopic, prob);
             string machineSrcState = machine + ":" + myState;
            
            if (!machineStateResponses.ContainsKey(machineSrcState))
             {
                 Hashtable h = new Hashtable ();
                 h[myTopic]=prob;
                 machineStateResponses[machineSrcState]=h;
             }
            ((Hashtable)machineStateResponses[machineSrcState])[myTopic]=prob;
        }

        public void addTransition(string machine, string srcState, string dstState, double prob)
        {
            RTPBot.writeDebugLine("MSM: addTransition({0},{1},{2},{3})", machine, srcState, dstState, prob);
            string machineSrcState = machine + ":" + srcState;
            string machineDstState = machine + ":" + dstState;
            addMachineState(machine, srcState);
            addMachineState(machine, dstState);

            try
            {

                double newSum = prob;
                if (transitionCounts.ContainsKey(machineSrcState)) { newSum = newSum + (double)transitionCounts[machineSrcState]; }
                transitionCounts[machineSrcState] = newSum;

                string atomicKey = machineSrcState + " " + machineDstState;
                atomicTransitionCount[atomicKey] = prob;
                transSum += prob;
            }
            catch
            {
                RTPBot.writeDebugLine("addTransition fail {0} {1} {2}", machineSrcState, machineDstState, prob);
            }
        }

        public void incrTransition(string machine, string srcState, string dstState, double prob)
        {
            RTPBot.writeDebugLine("MSM: incrTransition({0},{1},{2},{3})", machine, srcState, dstState, prob);
            string machineSrcState = machine + ":" + srcState;
            string machineDstState = machine + ":" + dstState;
            addMachineState(machine, srcState);
            addMachineState(machine, dstState);

            double incrP = prob;
            try
            {
                double newSum = prob;
                if (transitionCounts.ContainsKey(machineSrcState)) { newSum = newSum + (double)transitionCounts[machineSrcState]; }
                transitionCounts[machineSrcState] = newSum;


                string atomicKey = machineSrcState + " " + machineDstState;
                if (atomicTransitionCount.ContainsKey(atomicKey))
                {
                    double curP = (double)atomicTransitionCount[atomicKey];
                    atomicTransitionCount[atomicKey] = incrP + curP;
                }
                else
                {
                    atomicTransitionCount[atomicKey] = incrP;
                }
                transSum += incrP;

            }
            catch
            {
                RTPBot.writeDebugLine("incrTransition fail {0} {1} {2}", machineSrcState, machineDstState, prob);
            }
        }

        public double getTransitionProb(string machine, string srcState, string dstState)
        {
            RTPBot.writeDebugLine("MSM: getTransitionProb({0},{1},{2})", machine, srcState, dstState);
            string machineSrcState = machine + ":" + srcState;
            string machineDstState = machine + ":" + dstState;
            try
            {
                string atomicKey = machineSrcState + " " + machineDstState;
                double pSum = (double)transitionCounts[machineSrcState];
                double pNom = (double)atomicTransitionCount[atomicKey];
                double tp = (pNom / pSum);
                if (pSum < 0.9) { tp = tp * 0.1; }
                if (tp > 0.9) { tp = 0.9; }
                if (tp < 0.0001) { tp = 0.0001; }
                return tp;


            }
            catch
            {
                //RTPBot.writeDebugLine("   missing Transition {0} -- {1} ", srcState, dstState);
                return (double)0.0001;
            }
        }

        public void addEmission(string machine, string srcState, string evidence, double prob)
        {
            RTPBot.writeDebugLine("MSM: addEmission({0},{1},{2},{3})", machine, srcState, evidence, prob);
            lastDefEvidence = evidence;

            string machineSrcState = machine + ":" + srcState;
            addMachineState(machine, srcState);
            try
            {
                if (emissionProbability.ContainsKey(evidence))
                {
                    ((Hashtable)emissionProbability[evidence])[machineSrcState] = prob;
                }
                else
                {
                    Hashtable emitter = new Hashtable();
                    emitter.Add(machineSrcState, prob);
                    emissionProbability.Add(evidence, emitter);
                }
            }
            catch
            {
                RTPBot.writeDebugLine("addEmission fail {0} {1} {2}", machineSrcState, evidence, prob);
            }


        }

        public void addEvidenceState(string machine, string srcState, string evidence, double prob)
        {
            RTPBot.writeDebugLine("MSM: addEvidenceState({0},{1},{2},{3})", machine, srcState, evidence, prob);
            lastDefEvidence = evidence;

            string machineSrcState = machine + ":" + srcState;
            addMachineState(machine, srcState);
            try
            {
                if (evidenceStateProbability.ContainsKey(evidence))
                {
                    ((Hashtable)evidenceStateProbability[evidence])[machineSrcState] = prob;
                }
                else
                {
                    Hashtable emitter = new Hashtable();
                    emitter.Add(machineSrcState, prob);
                    evidenceStateProbability.Add(evidence, emitter);
                }
            }
            catch
            {
                RTPBot.writeDebugLine("addEvidenceState fail {0} {1} {2}", machineSrcState, evidence, prob);
            }


        }

        public void inspectEvidenceStates()
        {
            foreach (string evidence in evidenceStateProbability.Keys)
            {
                double evidenceSum = 0;
                Hashtable machineStateTable = ((Hashtable)evidenceStateProbability[evidence]);
                foreach (string state in machineStateTable.Keys)
                {
                    double eprob = (double)machineStateTable[state];
                    double stateprob = 0.00001;
                    try
                    {
                        stateprob = (double)cur_machineStateVal[state];
                    }
                    catch
                    {
                    }
                    double cprob = stateprob * eprob;
                    evidenceSum += cprob;
                }
                setEvidence(evidence, evidenceSum);
                // compute the negative evidence
                string negEvidence = "not-" + evidence;
                setEvidence(negEvidence, 1 - evidenceSum);
            }
        }

        public void machineDependency(string dependentMachine,string intialState, string controlmachine, string controlstate, double prob)
        {
            string disableState ="disabledstate";
            string disableMachineState = dependentMachine + ":" + disableState;
            string controlMachineState = controlmachine + ":" + controlstate;
            string controlEvidence = "evidence-disable-" + dependentMachine;
            string negcontrolEvidence = "not-evidence-disable-" + dependentMachine;

            // Create initial disable state and link to control state in foreign machine
            defState(dependentMachine, disableState, 0.9, 0.9);
            
            addEvidenceState(controlmachine, controlstate, controlEvidence, prob);

            addEmission(dependentMachine, disableState, controlEvidence, prob);
            addEmission(dependentMachine, intialState, negcontrolEvidence, 1-prob);

            addTransition(dependentMachine, disableState, intialState, 0.1);
            // Add transitions from each state to disabled state
            if (machineStates.ContainsKey(dependentMachine))
            {

                Hashtable statesHash = ((Hashtable)machineStates[dependentMachine]);
                foreach (string state in statesHash.Keys)
                {
                    //string machineState = dependentMachine + ":" + state;
                    //addTransition(dependentMachine, machineState, disableState, 0.1);
                    addTransition(dependentMachine, state, disableState, 0.1);
                }
            }
            
        }

        public void clearNextStateValues()
        {
            next_machineStateVal = new Hashtable();
        }

        public void advanceStateValues()
        {
            foreach (string k in next_machineStateVal.Keys)
            {
                cur_machineStateVal[k]=next_machineStateVal[k];
            }
        }

        public string advanceMachine(string machine)
        {
            if (!machineStates.ContainsKey(machine)) return "";
            // Cycle through the machine staes
            Hashtable statesHash = (Hashtable)machineStates[machine];
            string guessPath = "";
            int output = 0;
            
            // ok, start guessing
            string lastArgMax = "<idle>";

            // The storage mechanism
            double smoother = 0.00001; // 1 / transSum;

            // ensure a transition count for each state
            foreach (string stateKey in statesHash.Keys)
            {
                string mStateKey = machine + ":" + stateKey;
                if (!transitionCounts.ContainsKey(mStateKey)) transitionCounts[mStateKey] = (double)0.01;
            }

            // what is the emission probability ?
            // use a bayesian assumption and use the chain rule
            // under Zubek 
            //   Bt(s) = Sum_foreach Si in States{ p(S|Si) * q(Y|S) *Bt-1(Si) * Ns,t }
            // and q(Y|S) = Et(Y) * g(S|Y) *N
            // where E(Y) is evidence estimation
            //   and g(S|Y) is likelyhood of observing Y when in state S
            // see zubek dissertation p.148 Naive implementation
            //   for each target state S, iterate over all possible source states Si
            //    summing up previous  source state belief b(Si) at t-1 * transition prob p(S|Si)
            //    also sum up products of evidence g(Y|s) * evidence e(Y) over evidence types
            //    multiply the two gives raw belief estimate for state S
            //    normalize to get final belief value

            double bestVal = 0;
            string bestGuess = "";
            double Nsum = 0;

            ArrayList stateKeys = new ArrayList();
            foreach (string astate in statesHash.Keys)
            {
                string machineState = machine + ":" + astate;
                stateKeys.Add(machineState);
            }

            //foreach (string next_state in statesHash.Keys)
            //{
            //    string nextSrcMachineState = machine + ":" + next_state;

            foreach (string nextSrcMachineState in stateKeys)
            {
                double transitionChain = smoother;
                //foreach (string cur_state in statesHash.Keys)
                //{
                //    string curSrcMachineState = machine + ":" + cur_state;
                  foreach (string curSrcMachineState in stateKeys)
                  {
                    double bf_prob = smoother;
                    try
                    {
                        bf_prob = (double)cur_machineStateVal[curSrcMachineState]; 
                    }
                    catch
                    {
                    }
                    
                    // Our faster transition lookup
                    double transP = smoother; // should be smarter
                    string atomicKey = curSrcMachineState + " " + nextSrcMachineState;
                    if (atomicTransitionCount.ContainsKey(atomicKey))
                    {
                        transP = (double)atomicTransitionCount[atomicKey] / (double)transitionCounts[curSrcMachineState];

                    }
                    else
                    {
                        // use default smoother
                        transP = smoother;
                    }
                    double p = transP * bf_prob;

                    //RTPBot.writeDebugLine(" Transition: P({0}) = {1}", atomicKey, p);
                    transitionChain += p;
                }// for cur_state

                
                double evidenceChain = smoother;
                foreach (string evidenceToken in currentEvidence.Keys)
                {
                    double evidenceProb = (double)currentEvidence[evidenceToken];
                    double emitProb = smoother;
                    try
                    {
                        emitProb = (double)((Hashtable)emissionProbability[evidenceToken])[nextSrcMachineState];
                    }
                    catch
                    {
                    }
                    //RTPBot.writeDebugLine(" Evidence: P({0}) = {1} ", evidenceToken,evidenceProb);
                    //RTPBot.writeDebugLine("      EMP: P({0} | {1}) = {2} ", evidenceToken, nextSrcMachineState, emitProb);
                    //RTPBot.writeDebugLine("        G: {0} ", (emitProb * evidenceProb));

                    evidenceChain = evidenceChain + (emitProb * evidenceProb);
                }

                double rawBeliefInNextState = evidenceChain * transitionChain;
                next_machineStateVal[nextSrcMachineState] = rawBeliefInNextState;
                Nsum += rawBeliefInNextState;
                //RTPBot.writeDebugLine(" Next Raw P({0}) = {1} from tcp={2} and ecp={3}", nextSrcMachineState, rawBeliefInNextState, transitionChain, evidenceChain);

                if (rawBeliefInNextState > bestVal)
                {
                    bestGuess = nextSrcMachineState;
                    bestVal = rawBeliefInNextState;
                }
            }//for next_state

            // Normalize
            //if (Nsum != 0)
            //{
            //    foreach (string next_state in statesHash.Keys)
            //    {
            //        string nextSrcMachineState = machine + ":" + next_state;
            //       next_machineStateVal[nextSrcMachineState] = (double)next_machineStateVal[nextSrcMachineState]/Nsum;
            //    }
            //}
            normalizeMachineNextState(machine);

            RTPBot.writeDebugLine("MSM: The highest probability for machine {0} in state {1} [{2}]", machine, bestGuess, next_machineStateVal[bestGuess]);
            RTPBot.writeDebugLine("MSM: Guess = [{0}]", bestGuess);
            return bestGuess ;
        }

        public void normalizeMachineCurState(string machine)
        {
            if (!machineStates.ContainsKey(machine)) return;
            Hashtable statesHash = ((Hashtable)machineStates[machine]);
            double NSum = 0;
            foreach (string state in statesHash.Keys)
            {
                string machineState = machine + ":" + state;
                NSum += (double)cur_machineStateVal[machineState];
            }
            if (NSum != 0)
            {
                foreach (string state in statesHash.Keys)
                {
                    string machineState = machine + ":" + state;
                    cur_machineStateVal[machineState] = (double)cur_machineStateVal[machineState] / NSum;
                }

            }
        }

        public void normalizeMachineNextState(string machine)
        {
            if (!machineStates.ContainsKey(machine)) return;
            Hashtable statesHash = ((Hashtable)machineStates[machine]);
            double NSum = 0;
            foreach (string state in statesHash.Keys)
            {
                string machineState = machine + ":" + state;
                NSum += (double)next_machineStateVal[machineState];
            }
            if (NSum != 0)
            {
                foreach (string state in statesHash.Keys)
                {
                    string machineState = machine + ":" + state;
                    next_machineStateVal[machineState] = (double)next_machineStateVal[machineState] / NSum;
                }

            }
        }

    }
}