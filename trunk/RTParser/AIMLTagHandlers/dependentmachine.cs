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
    public class dependentmachine : RTParser.Utils.AIMLTagHandler
    {

        public dependentmachine(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "dependentmachine")
            {
                try
                {
                    string payload = templateNodeInnerText.ToValue(query);
                //<dependentmachine dependentmachine=""  initialstate="" 
                //                        controlmachine="" controlstate="" controlprob=""/>

                    string depMachine = GetAttribValue("dependentmachine", this.user.bot.pMSM.lastDefMachine);
                    string depState = GetAttribValue("initialstate", this.user.bot.pMSM.lastDefState);
                    string ctrlMachine = GetAttribValue("controlmachine", "m1");
                    string ctrlState = GetAttribValue("controlstate", "s1");
                    string ctrlProb = GetAttribValue("controlprob", "0.1");
                    double prob = double.Parse(ctrlProb);
                    this.user.bot.pMSM.machineDependency(depMachine, depState, ctrlMachine, ctrlState,prob);

                }
                catch
                {
                }
            }
            return Unifiable.Empty;

        }
    }
}