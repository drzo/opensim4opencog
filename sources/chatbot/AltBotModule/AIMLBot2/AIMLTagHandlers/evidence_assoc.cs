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
    public class evidence_assoc : RTParser.Utils.AIMLTagHandler
    {

        public evidence_assoc(RTParser.RTPBot bot,
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
            if (CheckNode("evidenceassoc"))
            {
                try
                {
                    var varMSM = this.botActionMSM;
                    string payload = templateNodeInnerText.ToValue(query);

                    string machine = GetAttribValue("machine", varMSM.lastDefMachine);
                    string myState = GetAttribValue("state", varMSM.lastDefState);
                    string myEvidence = GetAttribValue("evidence", varMSM.lastDefEvidence);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);
                    MachineSideEffect(() => varMSM.addEmission(machine, myState, myEvidence, prob));

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