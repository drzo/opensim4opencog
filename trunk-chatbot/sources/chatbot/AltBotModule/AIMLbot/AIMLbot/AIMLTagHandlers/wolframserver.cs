using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;

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
    public class wolframserver : AltAIMLbot.Utils.AIMLTagHandler
    {

        public wolframserver(AltAIMLbot.AltBot bot,
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
            if (this.templateNode.Name.ToLower() == "wolframserver")
            {
                string webAns = "Sorry, I don't understand.";
                // Simply push the filled in tag contents onto the stack
                try
                {
                    String templateNodeInnerValue = this.templateNode.InnerText;
                    string myUrl = this.templateNode.Attributes["url"].Value;
                    string myAppID = this.templateNode.Attributes["appid"].Value;
                    if (myUrl == null) { myUrl = bot.GlobalSettings.grabSetting("wolframserverurl"); }
                    if (myAppID == null) { myAppID = bot.GlobalSettings.grabSetting("wolframserverappid"); }

                    string query = templateNodeInnerValue;
                    query = query.Replace('\n', ' ');
                    query = query.Replace('\r', ' ');
                    query = query.Trim();

                    string webAsk = myUrl + "?format=plaintext&appid=" + myAppID + "&input=" + query;
                    Console.WriteLine("WEBQUERY:{0}", webAsk);
                    WebClient client = new WebClient();
                    string response = client.DownloadString(webAsk);
                    Console.WriteLine("WEBResponse:{0}", response);
                    response = response.Replace('\n', '.');
                    response = response.Replace('\r', ' ');

                    // We want <tk:text_result>(OUR ANSWER) </tk:result>
                    MatchCollection matchResult = Regex.Matches(response, @"\<plaintext\>(.*?)\<\/plaintext\>");
                    Match matchERRMessage = Regex.Match(response, @"\<error\>(.*?)\<\/error\>");
                    Match matchStatus = Regex.Match(response, @"\<status\>(.*?)\<\/status\>");
                    // Status: yes,no,complete,completeness unknown
                    if (matchResult.Count>0)
                    {
                        webAns = "";
                        foreach (Match match in matchResult)
                        {
                            for (int i = 0; i < match.Groups.Count; i++)
                            {

                                webAns += match.Groups[i + 1].Value + " ";
                                if ((webAns != null) && (webAns.Length > 0))
                                {
                                    // any answer post processing ?
                                }
                                else
                                {
                                    // webAns = "Sorry, I don't understand.";
                                }
                            }
                        }
                        webAns = webAns.Replace('|', ',');
                        webAns = webAns.Replace('\n', '.');
                        webAns = webAns.Trim();
                    }
                    else
                    {
                            webAns = "Sorry, I don't understand.";
                    }
                    Console.WriteLine("wolframserver :" + webAns);
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
