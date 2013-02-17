using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class evidence_pattern : AIMLTagHandlerU
    {

        public evidence_pattern(AltBot bot,
                User user,
                SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChangeU()
        {
            if (CheckNode("evidencepattern"))
            {
                try
                {
                    var varMSM = botActionMSM;

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
                    //Proc.AddAiml(evidenceCode);
                    GraphMaster myGraph = request.GetGraph("msm");
                    AddSideEffect("ADD AIML " + evidenceCode, () => Proc.AddAiml(myGraph, evidenceCode));

                    AltBot.writeDebugLine("MSM: evidence_pattern evidenceCode = {0}", evidenceCode);

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