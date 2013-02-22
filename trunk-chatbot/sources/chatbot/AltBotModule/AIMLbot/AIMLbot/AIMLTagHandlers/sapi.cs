using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Coursey, Daxtron Labs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

namespace AltAIMLbot.AIMLTagHandlers
{
    public class sapi : Utils.AIMLTagHandler
    {

        public sapi(AltBot bot,
                User user,
                Utils.SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
        {
            if (this.TemplateNodeName == "sapi")
            {
                // If there is a conversation memo then pop it
                // otherwise take the tag content as a srai (to trip say a random reply)

                try
                {
                    string message = "";
                    if (this.user.rbot.saySapi)
                    {
                        message = this.TemplateNodeOuterXml;
                        message = message.Replace(@"<sapi>", "");
                        message = message.Replace(@"</sapi>", "");
#if STANDALONE_TESTS
                        message = MyBehFacade.FixXmlEnitites(message);
#else
                        message = BehaviorTree.FixXmlEnitites(message);
#endif
                    }
                    else
                    {
                        message = this.Recurse();
                    }
                    return message;

                }
                catch
                {

                }

            }
            return String.Empty;

        }

    }
}
