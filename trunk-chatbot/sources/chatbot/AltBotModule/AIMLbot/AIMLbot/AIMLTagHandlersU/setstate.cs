using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class setstate : AIMLTagHandlerU
    {

        public setstate(AltBot bot,
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
            if (CheckNode("setstate"))
            {
                try
                {
                    var varMSM = botActionMSM;
                    string machine = GetAttribValue("machine", varMSM.lastDefMachine);
                    string name = GetAttribValue("name", varMSM.lastDefState);
                    string cur_prob_str = GetAttribValue("prob", "0.1");
                    double cur_prob = double.Parse(cur_prob_str);

                    MachineSideEffect(() => varMSM.setState(machine, name, cur_prob));
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