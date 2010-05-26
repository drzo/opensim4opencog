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
    public class setstate : RTParser.Utils.AIMLTagHandler
    {

        public setstate(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "setstate")
            {
                try
                {
                    string machine = GetAttribValue("machine", null);
                    string name = GetAttribValue("name", null);
                    string cur_prob_str = GetAttribValue("prob", "0.1");
                    double cur_prob = double.Parse(cur_prob_str);

                    this.user.bot.pMSM.setState(machine, name, cur_prob);
                }
                catch
                {

                }

            }
            return Unifiable.Empty;

        }
    }
}