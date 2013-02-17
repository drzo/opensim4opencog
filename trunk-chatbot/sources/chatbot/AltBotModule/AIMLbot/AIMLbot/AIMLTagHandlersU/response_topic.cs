using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class response_topic : AIMLTagHandlerU
    {

        public response_topic(AltBot bot,
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

            if (CheckNode("responsetopic"))
            {
                try
                {
                    var varMSM = botActionMSM;
                    string payload = templateNodeInnerText.ToValue(query);
                    string payload2 = Recurse();
                    string payload3 = InnerXmlText(templateNode);
                    string machine = GetAttribValue("machine",  varMSM.lastDefMachine);
                    string myState = GetAttribValue("state", varMSM.lastDefState);
                    string myTopic = GetAttribValue("topic", null);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);
                    payload= payload.Replace("rcategory", "category");
                    payload= payload.Replace("rpattern", "pattern");
                    string responseCode = "<aiml graph=\"msm\"> <topic name=\"" + myTopic + "\"> " + payload + " </topic> </aiml>";
                    AltBot.writeDebugLine("MSM: response_topic ResponseCode = {0}", responseCode);
                    MachineSideEffect(() => varMSM.addResponse(machine, myState, myTopic, prob));
                    // TODO: define machine-state -> topic
                    AddSideEffect("Add AIML " + responseCode, () => TargetBot.AddAiml(responseCode));

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