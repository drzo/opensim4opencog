using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class transition : AIMLTagHandler
    {

        public transition(AltBot bot,
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
            if (CheckNode("transition"))
            {
                var varMSM = botActionMSM;
                try
                {
                    string fromState = GetAttribValue("from", varMSM.lastDefState);
                    string toState = GetAttribValue("to", null);
                    string machine = GetAttribValue("machine", varMSM.lastDefMachine);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);

                    MachineSideEffect(() => varMSM.addTransition(machine, fromState, toState, prob));
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