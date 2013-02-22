using System;
using System.Xml;
using System.Text;
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
    class say : Utils.AIMLTagHandler
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
        public say(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = true;
        }

        protected override Unifiable ProcessChangeU()
        {
            if (this.TemplateNodeName == "say")
            {
                // Simply push the filled in tag contents onto the queue
                try
                {
                    if (this.TemplateNodeHasText)
                    {
                        // non atomic version of the node
                        string message = this.TemplateNodeInnerText;
                        //if (bot.saySapi) message = this.TemplateNodeInnerXml;
                        Console.WriteLine("  SayA msg1:{0}", message);
                        string lastOut = this.bot.getBBHash("TTSText");
                        // avoid repeats
                        if (message == lastOut)
                        {
                            return string.Empty;
                        }
                        //if (message.Length > 0) this.user.bot.sayProcessor(message);
                        this.bot.setBBHash("TTSText", message);
                        //this.user.bot.bbSetHash("TTSText", message);
                        Random Rgen = new Random();
                        int myUUID = Rgen.Next(Int32.MaxValue);
                        //this.user.bot.bbSetHash("TTSuuid", myUUID.ToString());
                        this.bot.setBBHash("TTSuuid", myUUID.ToString());
                        Console.WriteLine("sayResponse :{0}:{1}", myUUID.ToString(), message);
                        if ((message.Length > 0) && (this.user.rbot.sayProcessor != null))
                        {
                            //this.user.bot.sayProcessor(message);
                            this.user.rbot.postOutput(message);
                            // Mark the output time
                            this.user.rbot.myBehaviors.keepTime("lastchatoutput", RunStatus.Success);
                            this.user.rbot.myBehaviors.activationTime("lastchatoutput", RunStatus.Success);

                        }
                    }
                    else
                    {
                        string message = this.TemplateNodeInnerXml; //.InnerText;
                        string message2 = this.TemplateNodeInnerText;
                        Console.WriteLine("  SayB msg1:{0}", message);
                        Console.WriteLine("  SayB msg2:{0}", message2);
                        //if (message.Length > 0) this.user.bot.sayProcessor(message);
                        //if (message2.Length > 0) this.user.bot.sayProcessor(message2);
                        if (!string.IsNullOrEmpty(message)) this.user.rbot.postOutput(message);
                        if (!string.IsNullOrEmpty(message2)) this.user.rbot.postOutput(message2);
                        // Mark the output time
                        this.user.rbot.myBehaviors.keepTime("lastchatoutput", RunStatus.Success);
                        this.user.rbot.myBehaviors.activationTime("lastchatoutput", RunStatus.Success);
                    }
                }
                catch(Exception e)
                {
                    Console.WriteLine("  Say Exception:{0}", e.Message );
                }

            }
            return string.Empty;
        }

    }
}
