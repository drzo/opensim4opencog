using System;
using System.Collections.Generic;
using System.Collections;
//using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using AltAIMLbot.Utils;
using RTParser;

/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Coursey, Daxtron Labs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

namespace AltAIMLbot
{
    [Serializable]
    public class QfsmSet
    {
        public Dictionary<string,Qfsm> machines;
        public QfsmSet()
        {
            machines = new Dictionary<string, Qfsm>();
        }
        public void defineMachine(string machine, string stateDef)
        {
            try
            {
                Qfsm newMachine = new Qfsm();
                newMachine.defineMachine(machine, stateDef);
                machines[machine] = newMachine;
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
            }
        }

        public void runBotMachines(AltBot bot)
        {
            foreach (string machineName in machines.Keys )
            {
                try
                {
                    Qfsm curMachine = (Qfsm)machines[machineName];
                    curMachine.advanceBotMachine(bot);
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
            }

        }

    }

    [Serializable]
    public class Qfsm
    {
        public XmlDocument rulesDoc;
        public string curState;
        public string initialState;
        public Int32 transitionTime;
        public Random rgen = new Random();
        public string name;

        private String ruleMatch = "//state[@id='{0}']";
        private String allTransitionsInARuleMatch = "transition";
        private String specificTransitionInARuleBasedOnEventMatch = "transition[@event={0}]";
        private String dataEventMatch = "transition[@event={0}]";
        private String allDataEventsInATagMatch = "send";
        private String allOnEntryInARuleMatch = "onentry";
        private String allOnExitInARuleMatch = "onexit";

        public Qfsm()
        {
            rulesDoc = new XmlDocument();
            transitionTime = Environment.TickCount;
        }

        public void defineMachine(string mname,string stateDef)
        {
            try
            {
                name = mname;
                stateDef = stateDef.Replace("&gt;", ">");
                stateDef = stateDef.Replace("&lt;", "<");

                rulesDoc.LoadXml(stateDef);
                initialState = rulesDoc.FirstChild.Attributes["initialstate"].Value;
                transitionTime = Environment.TickCount;
                curState = initialState;
                //XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, initialState));
            }
            catch (Exception e)
                {
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
        }

        public void advanceBotMachine(AltBot bot)
        {
            string nextState = curState;
            XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, curState));
            XmlNode node = rules[0];
            XmlNode lastTransition = null;
            Int32 elapsedTime = Environment.TickCount-transitionTime;

            foreach (XmlNode transition in node.SelectNodes(allTransitionsInARuleMatch))
            {
                string targetStateName = transition.Attributes["target"].Value;
                string eventData = transition.Attributes["event"] == null ? null : transition.Attributes["event"].Value;
                string condData = transition.Attributes["cond"] == null ? null : transition.Attributes["cond"].Value;
                string [] parms = condData.Trim().Split(' ');
                string varName = parms[0];
                string rel = parms[1].ToLower();
                string val = parms[2];
                double dval = 0;
                try
                {
                    dval = double.Parse(val);
                }
                catch (Exception e)
                {
                    dval = 0;
                }
                // Fetch conditional test
                // Checking blackboard/cache but should include 
                // bot predicates,cache, chemistry
                string sv = null;
                double bbVal = 0;
                try
                {
                    //sv = myChemistry.m_cBus.getHash("mdollhearduuid");
                    sv = bot.getBBHash(varName);
                    if (!string.IsNullOrEmpty(sv)) bbVal = double.Parse(sv);
                }
                catch (Exception e) { }

                // Special variables?
                if (varName == "timeout") bbVal = elapsedTime;
                if (varName == "prob") bbVal = rgen.NextDouble();

                // Check Condition
                bool valid = false;
                Match match = null;
                switch (rel)
                {
                    case "==":
                        valid = (bbVal == dval);
                        break;
                    case "<":
                        valid = (bbVal < dval);
                        break;
                    case ">":
                        valid = (bbVal > dval);
                        break;
                    case "lt":
                        valid = (bbVal < dval);
                        break;
                    case "gt":
                        valid = (bbVal > dval);
                        break;
                    case "<=":
                        valid = (bbVal <= dval);
                        break;
                    case ">=":
                        valid = (bbVal >= dval);
                        break;
                    case "matches":
                         match = (new Regex(val)).Match(sv);
                        valid = match.Success;
                        break;
                    case "!matches":
                         match = (new Regex(val)).Match(sv);
                        valid = !match.Success;
                        break;
                    case "notmatches":
                         match = (new Regex(val)).Match(sv);
                        valid = !match.Success;
                        break;
                   case "=~":
                         match = (new Regex(val)).Match(sv);
                        valid = match.Success;
                        break;
                    case "!~":
                         match = (new Regex(val)).Match(sv);
                        valid = !match.Success;
                        break;
                }
                
                if (varName == "TRUE") valid = true;

                // Stop on first so store in priority order
                // The alternative is to keep a success stack
                if (valid)
                {
                    lastTransition = transition;
                    nextState = targetStateName;
                    break;
                }
            }
            // Quit on no transition
            if (nextState == curState) return; 
            Console.WriteLine("FSM: {0}-->{1}", curState, nextState);
            // Run OnExit
            processOnExit(curState,bot);
            //Run Transition
            processTransition(lastTransition, bot);
            // Run OnEntry
            processOnEntry(nextState,bot);
            // Transition
            curState = nextState;
            transitionTime = Environment.TickCount;
            bot.setBBHash("fsmstate",curState);
        }

        public void processTransition(XmlNode transitionNode, AltBot bot)
        {
            Console.WriteLine("FSM: processTransition {0}", transitionNode.InnerText);

            foreach (XmlNode templateNode in transitionNode.ChildNodes)
            {
                bot.evalTemplateNode(templateNode);
            }
        }

        public void processOnEntry(string state, AltBot bot)
        {
            Console.WriteLine("FSM: processOnEntry {0}", state);
            XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, state));
            XmlNode node = rules[0];
            foreach (XmlNode entry in node.SelectNodes(allOnEntryInARuleMatch))
            {
                foreach (XmlNode templateNode in entry.ChildNodes)
                {
                    try
                    {
                        XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + templateNode.InnerXml + "</template>");
                        bot.evalTemplateNode(resultTemplateNode);
                        //bot.evalTemplateNode(templateNode);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("ERR: ProcessTask");
                        Console.WriteLine("ERR:" + e.Message);
                        Console.WriteLine("ERR:" + e.StackTrace);
                        Console.WriteLine("ERR XML:" + templateNode.OuterXml);
                        //result = RunStatus.Failure;
                    }
                }
            }
        }

        public void processOnExit(string state, AltBot bot)
        {
            Console.WriteLine("FSM: processOnExit {0}", state);
            XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, state));
            XmlNode node = rules[0];
            foreach (XmlNode entry in node.SelectNodes(allOnExitInARuleMatch))
            {
                foreach (XmlNode templateNode in entry.ChildNodes)
                {
                    try
                    {
                        XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + templateNode.InnerXml + "</template>");
                        bot.evalTemplateNode(resultTemplateNode);
                        //bot.evalTemplateNode(templateNode);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("ERR: ProcessTask");
                        Console.WriteLine("ERR:" + e.Message);
                        Console.WriteLine("ERR:" + e.StackTrace);
                        Console.WriteLine("ERR XML:" + templateNode.OuterXml);
                        //result = RunStatus.Failure;
                    }
                }

            }

        }
    }
}
