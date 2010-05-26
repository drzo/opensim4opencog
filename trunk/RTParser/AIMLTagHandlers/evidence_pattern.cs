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
    public class evidence_pattern : RTParser.Utils.AIMLTagHandler
    {

        public evidence_pattern(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "evidencepattern")
            {
                try
                {
                    string payload = templateNodeInnerText.ToValue();

                    string evidence = GetAttribValue("evidence", this.user.bot.pMSM.lastDefEvidence );
                    string prob_str = GetAttribValue("prob", "1.0");
                    double prob = double.Parse(prob_str);

                    string evidenceCode = "<category><pattern>"+payload+"</pattern>"+
                                         "<template><think><setevidence evidence=\""+evidence+"\" prob="+prob_str+" />"+
                                         "</think></template></category>";
                    this.user.bot.AddAiml(evidenceCode);
                    Console.WriteLine("MSM: evidence_pattern evidenceCode = {0}", evidenceCode);

                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}