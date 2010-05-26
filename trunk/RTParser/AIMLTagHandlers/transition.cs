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
    public class transition : RTParser.Utils.AIMLTagHandler
    {

        public transition(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "transition")
            {
                try
                {
                    string fromState = GetAttribValue("from", null);
                    string toState = GetAttribValue("to", null);
                    string machine = GetAttribValue("machine", null);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);

                    this.user.bot.pMSM.addTransition(machine, fromState ,toState ,prob );
                }
                catch
                {
                }
            }
            return Unifiable.Empty;

        }
    }
}