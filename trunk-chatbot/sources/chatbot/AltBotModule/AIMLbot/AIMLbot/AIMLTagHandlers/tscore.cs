using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class tscore : AIMLTagHandler
    {

        public tscore(AltBot bot,
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
                    string str = SafeFormat("TSCORE {0}<-{1}*{2}", newrating, beforerating, templateScore);
                    return Succeed(str);
                }
                catch (Exception e)
                {
                    writeToLogWarn("" + e);
                }

            }
            return Unifiable.Empty;

        }
    }
}