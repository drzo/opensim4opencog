using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class msm : AIMLTagHandlerU
    {

        public msm(AltBot bot,
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
            if (CheckNode("msm"))
            {
                var varMSM = botActionMSM;
                try
                {
                    string machine = GetAttribValue("machine", null);
                    MachineSideEffect(() => varMSM.addMachine(machine));
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