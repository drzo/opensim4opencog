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
    public class evidencestate : RTParser.Utils.AIMLTagHandler
    {

        public evidencestate(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "evidencestate")
            {
                try
                {
                    var varMSM = this.botActionMSM;
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