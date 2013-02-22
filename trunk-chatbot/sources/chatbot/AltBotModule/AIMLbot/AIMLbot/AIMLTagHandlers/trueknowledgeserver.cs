using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;
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
    public class trueknowledgeserver : Utils.AIMLTagHandler
    {

        public trueknowledgeserver(AltBot bot,
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
            if (this.TemplateNodeName == "trueknowledgeserver")
            {
                string webAns = "Sorry, I don't understand.";
                // Simply push the filled in tag contents onto the stack
                try
                {
                    String templateNodeInnerValue = this.TemplateNodeInnerText;
                    string myUrl = TemplateNodeAttributes["url"].Value;
                    string myUser = TemplateNodeAttributes["apiuser"].Value;
                    string myPass = TemplateNodeAttributes["apipass"].Value;
                    if (myUrl == null) { myUrl = bot.GlobalSettings.grabSetting("trueknowledgeserverurl"); }
                    if (myUser == null) { myUser = bot.GlobalSettings.grabSetting("trueknowledgeserveruser"); }
                    if (myPass == null) { myPass = bot.GlobalSettings.grabSetting("trueknowledgeserverpass"); }



                    string query = templateNodeInnerValue;
                    query = query.Replace('\n', ' ');
                    query = query.Replace('\r', ' ');
                    query = query.Trim();
                    
                    string webAsk = myUrl+"?api_account_id="+myUser+"&api_password="+myPass+"&question=" + query;
                    bot.logText(String.Format("WEBQUERY:{0}", webAsk));
                    WebClient client = new WebClient();
                    string response = client.DownloadString(webAsk);
                    bot.logText(String.Format("WEBResponse:{0}", response));
                    // We want <tk:text_result>(OUR ANSWER) </tk:result>
                    Match matchResult = Regex.Match(response, @"\<tk\:text_result\>(.*?)\<\/tk\:text_result\>");
                    Match matchERRMessage = Regex.Match(response, @"\<tk\:error_message\>(.*?)\<\/tk\:error_message\>");
                    Match matchStatus = Regex.Match(response, @"\<tk\:status\>(.*?)\<\/tk\:status\>");
                    // Status: yes,no,complete,completeness unknown
                    if (matchResult.Success)
                    {
                        webAns = matchResult.Groups[1].Value;
                        if ((webAns != null) && (webAns.Length > 0))
                        {
                            // any answer post processing ?
                        }
                        else
                        {
                            if (matchERRMessage.Success)
                            {
                                webAns = matchERRMessage.Groups[1].Value; 
                            }
                            else
                            {
                                webAns = "Sorry, I don't understand.";
                            }
                        }
                    }
                    else
                    {
                        if (matchERRMessage.Success)
                        {
                            webAns = matchERRMessage.Groups[1].Value;
                        }
                        else
                        {
                            webAns = "Sorry, I don't understand.";
                        }
                    }
                    bot.logText(String.Format("trueknowledgeserver :" + webAns));
                    return webAns;
                }
                catch(Exception e)
                {
                    bot.logText(String.Format("ERR: {0} {1}", e.Message, e.StackTrace));
                    webAns = "Processing caused the following error. " + e.Message;
                    return webAns;
                }

            }
            return String.Empty;

        }

    }
}
