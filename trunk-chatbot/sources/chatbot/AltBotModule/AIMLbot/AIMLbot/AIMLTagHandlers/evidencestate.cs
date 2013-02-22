using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class evidencestate : AIMLTagHandler
    {

        public evidencestate(AltBot bot,
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
            if (templateNode.Name.ToLower() == "evidencestate")
            {
                try
                {
                    var varMSM = botActionMSM;
                    string payload = templateNodeInnerText.ToValue(query);

                    string myMachine = GetAttribValue("machine", varMSM.lastDefMachine);
                    string myState = GetAttribValue("state", varMSM.lastDefState);
                    string myEvidence = GetAttribValue("evidence", varMSM.lastDefEvidence);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);
                    MachineSideEffect(() => varMSM.addEvidenceState(myMachine, myState, myEvidence, prob));

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