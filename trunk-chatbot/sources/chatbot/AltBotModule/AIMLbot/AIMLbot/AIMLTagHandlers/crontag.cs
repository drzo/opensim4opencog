using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;
/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Courssey, Daxtron Labs

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
    public class crontag : AltAIMLbot.Utils.AIMLTagHandler
    {
                       /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public crontag(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override string ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "crontag")
            {
                // Simply push the filled in tag contents onto the queue
                try
                {
                    String templateInnerValue = this.templateNode.InnerXml;
                    String myTimeSpec = "* * * * * *";
                    try
                    {
                        myTimeSpec = this.templateNode.Attributes["timeline"].Value;
                    }
                    catch(Exception)
                    {
                        myTimeSpec = "* * * * * *";
                    }

                    String myBehavior = "root";
                    try
                    {
                        myBehavior = this.templateNode.Attributes["id"].Value;
                    }
                    catch (Exception)
                    {
                        myBehavior = "root";
                    }
                    String myTimeMode = "absolute";
                    try
                    {
                        if (this.templateNode.Attributes["mode"] != null)
                        {
                            myTimeMode = this.templateNode.Attributes["mode"].Value;
                        }
                    }
                    catch (Exception)
                    {
                        myTimeMode = "absolute";
                    }

                    String myTimeLine = String.Format("{0} {1} {2} {3}", myTimeSpec,myTimeMode, myBehavior, templateInnerValue);
                    this.user.bot.myCron.addLine(myTimeLine);
                }
                catch (Exception)
                {
                }

            }
            return string.Empty;
        }

    }
}
