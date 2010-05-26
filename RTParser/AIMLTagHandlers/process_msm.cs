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
            }
            return Unifiable.Empty;

        }
    }

    public class actMSM
    {
        // emission and transition stored as double hash tables
        public Hashtable machines = new Hashtable(); // just the machines
        public Hashtable emissionProbability = new Hashtable();
        public Hashtable transitionCounts = new Hashtable();
        public Hashtable stateToEvidence = new Hashtable(); // machine:state -> evidence, prob
        public Hashtable atomicTransitionCount = new Hashtable();
        public Hashtable currentEvidence = new Hashtable(); // just the current evidence

        public Hashtable machineStateResponses = new Hashtable(); // machine:state -> response,prob
        public Hashtable machineResponses = new Hashtable(); // just list of machine-> possibe_responses

        public Hashtable cur_machineStateVal = new Hashtable();
        public Hashtable next_machineStateVal = new Hashtable();

        public double transSum = 0.0001;

        static actMSM()
        {
        }

        public void addMachine(string machine)
        {
            Console.WriteLine("MSM: addMachine({0})", machine);
            machines[machine] = 1;
        }

        public void setState(string machine, string state, double prob)
        {
            Console.WriteLine("MSM: setState({0},{1},{2})", machine, state, prob);
            string machineState = machine + ":" + state;
             cur_machineStateVal[machineState] = prob;
        }
        public void defState(string machine, string state, double initProb, double selfProb)
        {
            Console.WriteLine("MSM:   defState({0},{1},{2},{3})", machine, state, initProb, selfProb);
            machines[machine] = 1;
            string machineState = machine + ":" + state;
            setState(machine, state, initProb);
            addTransition(machine, state, state, selfProb);

        }
        public void clearEvidence()
        {
            Console.WriteLine("MSM:  clearEvidence()");
            currentEvidence.Clear();
        }
        public void setEvidence(string evidence, double evidenceProb)
        {
            Console.WriteLine("MSM:  setEvidence({0},{1})", evidence, evidenceProb);
            currentEvidence.Add(evidence, evidenceProb);
        }
        public void addResponse(string machine, string myState,string myTopic, double prob)
        {
            Console.WriteLine("MSM: **** TODO ***  addResponse({0},{1},{2},{3})", machine, myState, myTopic, prob);
        }

        public void addTransition(string machine, string srcState, string dstState, double prob)
        {
            Console.WriteLine("MSM: addTransition({0},{1},{2},{3})", machine, srcState, dstState, prob);
            string machineSrcState = machine + ":" + srcState;
            string machineDstState = machine + ":" + dstState;

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
                Console.WriteLine("addTransition fail {0} {1} {2}", machineSrcState, machineDstState, prob);
            }
        }

        public void incrTransition(string machine, string srcState, string dstState, double prob)
        {
            Console.WriteLine("MSM: incrTransition({0},{1},{2},{3})", machine, srcState, dstState, prob);
            string machineSrcState = machine + ":" + srcState;
            string machineDstState = machine + ":" + dstState;

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
                Console.WriteLine("incrTransition fail {0} {1} {2}", machineSrcState, machineDstState, prob);
            }
        }

        public double getTransitionProb(string machine, string srcState, string dstState)
        {
            Console.WriteLine("MSM: getTransitionProb({0},{1},{2})", machine, srcState, dstState);
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
                //Console.WriteLine("   missing Transition {0} -- {1} ", srcState, dstState);
                return (double)0.0001;
            }
        }

        public void addEmission(string machine, string srcState, string evidence, double prob)
        {
            Console.WriteLine("MSM: addEmission({0},{1},{2},{3})", machine, srcState, evidence, prob);
            string machineSrcState = machine + ":" + srcState;
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
                Console.WriteLine("addEmission fail {0} {1} {2}", machineSrcState, evidence, prob);
            }


        }
    }
}