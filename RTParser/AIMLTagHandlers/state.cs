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
    public class state : RTParser.Utils.AIMLTagHandler
    {

        public state(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "state")
            {
                try
                {
                    string payload = templateNodeInnerText.ToValue(query);
                    string payload3 = templateNode.InnerXml;

                    string state = GetAttribValue("name", null);
                    string machine = GetAttribValue("machine", this.user.bot.pMSM.lastDefMachine);
                    string init_prob_str = GetAttribValue("init_prob", "0.1");
                    string self_prob_str = GetAttribValue("self_prob", "0.1");
                    double init_prob = double.Parse(init_prob_str);
                    double self_prob = double.Parse(self_prob_str);
                    this.user.bot.pMSM.lastDefState = state;

                    this.user.bot.pMSM.defState(machine, state, init_prob, self_prob);

                    string responseCode = "<aiml graph=\"msm\"> " + payload3 + " </aiml>";
                    this.user.bot.AddAiml(responseCode);
                }
                catch
                {
                }
            }
            return Unifiable.Empty;

        }
    }
}