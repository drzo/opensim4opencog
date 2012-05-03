using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Xml;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;
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

/*
 * Uses Pannous AIML bot server for question answering.
 * Pannous returns JSON
 * https://docs.google.com/document/d/1dVG_B5Sc2x-fi1pN6iJJjfF1bJY6KEFzUqjOb8NsntI/edit?pli=1# 
 * 
 * <category>
 * <pattern>SEARCH WEATHER *</pattern>
 * <template>
 * <pannouserver url="http://botecho.pannous.com/bot1" login="test-user" key="">weather <star/></pannouserver>
 * </template>
 * </category>
 * 
 */

namespace AltAIMLbot.AIMLTagHandlers
{
    public class pannouserver : AltAIMLbot.Utils.AIMLTagHandler
    {

        public pannouserver(AltAIMLbot.AltBot bot,
                AltAIMLbot.User user,
                AltAIMLbot.Utils.SubQuery query,
                AltAIMLbot.Request request,
                AltAIMLbot.Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override String ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "pannouserver")
            {
                string webAns = "Sorry, I don't understand.";
                // Simply push the filled in tag contents onto the stack
                try
                {
                    String templateNodeInnerValue = this.templateNode.InnerText;
                    string myUrl = this.templateNode.Attributes["url"].Value;
                    string myLogin = this.templateNode.Attributes["login"].Value;
                    string myKey = this.templateNode.Attributes["key"].Value;
                    string query = templateNodeInnerValue;
                    query = query.Replace('\n', ' ');
                    query = query.Replace('\r', ' ');
                    query = query.Trim();

                    string webAsk = myUrl + "?clientFeatures=say&login=" + myLogin + "&key=" + myKey + "&input=" + query;
                    Console.WriteLine("WEBQUERY:{0}", webAsk);
                    WebClient client = new WebClient();
                    string response = client.DownloadString(webAsk);
                    Console.WriteLine("WEBResponse:{0}", response);
                    // We want <tk:text_result>(OUR ANSWER) </tk:result>
                    string finalSay = "";
                    Hashtable top = (Hashtable)JSON.JsonDecode(response);
                    if (top.ContainsKey("output"))
                    {
                        //Hashtable info = (Hashtable)top["info"];
                        ArrayList output = (ArrayList)top["output"];
                        if (output.Count > 0)
                        {
                            Hashtable output1 = (Hashtable)output[0];
                            if (output1.ContainsKey("actions"))
                            {
                                Hashtable actions = (Hashtable)output1["actions"];
                                if (actions.ContainsKey ("say"))
                                {
                                    finalSay = (string)actions["say"];
                                }
                            }

                        }
                    }
                   
                    if (finalSay.Length>1)
                    {
                        webAns = finalSay;
                        if ((webAns != null) && (webAns.Length > 0))
                        {
                            // any answer post processing ?
                        }
                        else
                        {

                                webAns = "Sorry, I don't understand.";
                            }

                    }
                    else
                    {

                            webAns = "Sorry, I don't understand.";

                    }
                    Console.WriteLine("pannouserver :" + webAns);
                    return webAns;
                }
                catch(Exception e)
                {
                    Console.WriteLine("ERR: {0} {1}", e.Message, e.StackTrace);
                    webAns = "Processing caused the following error. " + e.Message;
                    return webAns;
                }

            }
            return String.Empty;

        }

    }
}
