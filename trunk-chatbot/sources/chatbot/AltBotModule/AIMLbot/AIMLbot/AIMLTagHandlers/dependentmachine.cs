using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class dependentmachine : AIMLTagHandlerU
    {

        public dependentmachine(AltBot bot,
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
            if (CheckNode("dependentmachine"))
            {
                try
                {
                    string payload = templateNodeInnerText.ToValue(query);
                //<dependentmachine dependentmachine=""  initialstate="" 
                //                        controlmachine="" controlstate="" controlprob=""/>

                    var varMSM = botActionMSM;
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