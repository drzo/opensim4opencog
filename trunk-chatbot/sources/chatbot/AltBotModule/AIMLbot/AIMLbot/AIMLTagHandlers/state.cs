using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class state : AIMLTagHandler
    {

        public state(AltBot bot,
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
            if (CheckNode("state"))
            {
                try
                {
                    var varMSM = botActionMSM;
                    string payload = templateNodeInnerText.ToValue(query);
                    string payload3 = InnerXmlText(templateNode);
                    string prevLastDefMachine = varMSM.lastDefMachine;
                    string state = GetAttribValue("name", null);
                    string machine = GetAttribValue("machine", prevLastDefMachine);
                    string init_prob_str = GetAttribValue("init_prob", "0.1");
                    string self_prob_str = GetAttribValue("self_prob", "0.1");
                    double init_prob = double.Parse(init_prob_str);
                    double self_prob = double.Parse(self_prob_str);
                    varMSM.lastDefState = state;
                    MachineSideEffect(() =>
                                          {
                                              varMSM.lastDefState = prevLastDefMachine;
                                              varMSM.defState(machine, state, init_prob, self_prob);
                                              string responseCode = "<aiml graph=\"msm\"> " + payload3 + " </aiml>";
                                              AddSideEffect("Add AIML " + responseCode, () => TargetBot.AddAiml(responseCode));
                                          });
                        
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