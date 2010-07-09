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
                    string payload = templateNodeInnerText.ToValue();

                    string myMachine = GetAttribValue("machine", this.user.bot.pMSM.lastDefMachine);
                    string myState = GetAttribValue("state", this.user.bot.pMSM.lastDefState);
                    string myEvidence = GetAttribValue("evidence", this.user.bot.pMSM.lastDefEvidence);
                    string prob_str = GetAttribValue("prob", "0.1");
                    double prob = double.Parse(prob_str);
                    this.user.bot.pMSM.addEvidenceState(myMachine, myState, myEvidence, prob);

                }
                catch
                {
                }
            }
            return Unifiable.Empty;

        }
    }
}