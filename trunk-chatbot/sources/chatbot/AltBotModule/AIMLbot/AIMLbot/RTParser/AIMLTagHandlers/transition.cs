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
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using RTParser;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    public class transition : RTParser.Utils.AIMLTagHandler
    {

        public transition(RTParser.AltBot bot,
                User user,
                SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChange()
        {
            if (CheckNode("transition"))
            {
                var varMSM = this.botActionMSM;
                try
                {
                    string fromState = GetAttribValue("from", varMSM.lastDefState);
                    string toState = GetAttribValue("to", null);
                    string machine = GetAttribValue("machine", varMSM.lastDefMachine);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);

                    MachineSideEffect(() => varMSM.addTransition(machine, fromState, toState, prob));
                }
                catch (Exception e)
                {
                    writeToLogWarn("MSMWARN: " + e);
                }
            }
            return Unifiable.Empty;

        }
    }
}