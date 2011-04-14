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
    public class tscore : RTParser.Utils.AIMLTagHandler
    {

        public tscore(RTParser.RTPBot bot,
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
            if (CheckNode("tscore"))
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    double templateScore = GetAttribValue<double>(templateNode, "mult", () => (1.0),
                                                          ReduceStarAttribute<double>);
                    double beforerating = request.TopLevelScore;
                    double newrating = beforerating * templateScore;
                    request.TopLevelScore = newrating;
                    if (false)
                    {
                        string str = SafeFormat("TSCORE {1}<-{2}*{3}", newrating, beforerating, templateScore);
                        writeToLog(str);
                    }
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}