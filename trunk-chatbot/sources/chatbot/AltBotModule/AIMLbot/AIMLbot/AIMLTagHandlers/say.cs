using System;
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
    class say : AltAIMLbot.Utils.AIMLTagHandler
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
        public say(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = true;
        }

        protected override string ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "say")
            {
                // Simply push the filled in tag contents onto the queue
                try
                {
                    if (this.templateNode.InnerText.Length > 0)
                    {
                        // non atomic version of the node
                        string message = this.templateNode.InnerText;
                        Console.WriteLine("  SayA msg1:{0}", message);
                        //if (message.Length > 0) this.user.bot.sayProcessor(message);
                        this.bot.setBBHash("TTSText", message);
                        //this.user.bot.myChemistry.m_cBus.setHash("TTSText", message);
                        Random Rgen = new Random();
                        int myUUID = Rgen.Next(Int32.MaxValue);
                        //this.user.bot.myChemistry.m_cBus.setHash("TTSuuid", myUUID.ToString());
                        this.bot.setBBHash("TTSuuid", myUUID.ToString());
                        Console.WriteLine("sayResponse :{0}:{1}", myUUID.ToString(), message);
                        if ((message.Length > 0) && (this.user.bot.sayProcessor != null))
                        {
                            this.user.bot.sayProcessor(message);
                        }
                    }
                    else
                    {
                        string message = this.templateNode.InnerXml; //.InnerText;
                        string message2 = this.templateNode.InnerText;
                        Console.WriteLine("  SayB msg1:{0}", message);
                        Console.WriteLine("  SayB msg2:{0}", message2);
                        if (message.Length > 0) this.user.bot.sayProcessor(message);
                        if (message2.Length > 0) this.user.bot.sayProcessor(message2);
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
