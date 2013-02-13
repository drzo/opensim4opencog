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
    public class msm : RTParser.Utils.AIMLTagHandler
    {

        public msm(RTParser.RTPBot bot,
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
            if (CheckNode("msm"))
            {
                var varMSM = this.botActionMSM;
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