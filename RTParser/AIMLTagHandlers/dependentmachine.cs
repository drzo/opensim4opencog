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
            if (CheckNode("dependentmachine"))
            {
                try
                {
                    string payload = templateNodeInnerText.ToValue(query);
                //<dependentmachine dependentmachine=""  initialstate="" 
                //                        controlmachine="" controlstate="" controlprob=""/>

                    var varMSM = this.botActionMSM;
                    string depMachine = GetAttribValue("dependentmachine", varMSM.lastDefMachine);
                    string depState = GetAttribValue("initialstate", varMSM.lastDefState);
                    string ctrlMachine = GetAttribValue("controlmachine", "m1");
                    string ctrlState = GetAttribValue("controlstate", "s1");
                    string ctrlProb = GetAttribValue("controlprob", "0.1");
                    double prob = double.Parse(ctrlProb);
                    MachineSideEffect(() => varMSM.machineDependency(depMachine, depState, ctrlMachine, ctrlState, prob));

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