using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class setevidence : AIMLTagHandler
    {

        public setevidence(AltBot bot,
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
            if (templateNode.Name.ToLower() == "setevidence")
            {
                try

                {
                    var varMSM = botActionMSM;
                    string name = GetAttribValue("evidence", null);
                    string cur_prob_str = GetAttribValue("prob", "0.1");
                    double cur_prob = double.Parse(cur_prob_str);

                    MachineSideEffect(() => varMSM.setEvidence(name, cur_prob));
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