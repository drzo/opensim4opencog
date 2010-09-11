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
            if (CheckNode("evidencepattern"))
            {
                try
                {
                    var varMSM = this.botActionMSM;

                    string payload = templateNodeInnerText.ToValue(query);

                    string evidence = GetAttribValue("evidence", varMSM.lastDefEvidence );
                    string prob_str = GetAttribValue("prob", "1.0");
                    double prob = double.Parse(prob_str);
                    //string quote = "" + '\u0022' + "";
                    string quote = "'";
                    string evidenceCode =
                                         //@"<aiml graph=" + quote + "msm" + quote + ">" +
                                         @"<topic name=" + quote + "CEP" + quote + " > <category><pattern>" + payload + "</pattern>" +
                                         @"<template><think><setevidence evidence=" + quote + evidence + quote +
                                         @" prob=" + quote + prob_str + quote +
                                         @" /></think>ep "+evidence+"</template></category></topic>";
                                         //@"</aiml>";
                    //this.user.bot.AddAiml(evidenceCode);
                    GraphMaster myGraph = Proc.GetGraph("msm", request.Graph);
                    AddSideEffect("ADD AIML " + evidenceCode, () => this.user.bot.AddAiml(myGraph, evidenceCode));

                    RTPBot.writeDebugLine("MSM: evidence_pattern evidenceCode = {0}", evidenceCode);

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