using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Xml;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;
using AltAIMLbot.Utils;
using AltAIMLParser;
using RTParser;

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
 * Uses siProlog to generate text
 * <category>
 * <pattern>WHAT DO YOU THINK</pattern>
 * <template>
 * <logictext mt="myThoughtsMt" choice="random">cansay(TEXT)</logictext>
 * </template>
 * </category>
 * 
 */

namespace AltAIMLbot.AIMLTagHandlers
{
    public class logictext : Utils.AIMLTagHandler
    {

        public logictext(AltBot bot,
                User user,
                Utils.SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = true;
            IsStarAtomically = true;
            isBoring = true;
        }

        protected override String ProcessChange()
        {
             Random rgen = new Random();
           if (this.TemplateNodeName == "logictext")
            {
                string logicAns = "Sorry, I don't understand.";
                // <logictext mt="mymt" choice="random|x">cansay(TEXT)</logictext>
                // will prefrom query in mt and return text string
                // mt=query mt
                // choice=random | all
                // innerText = query to run. "TEXT" will indicate bound variable to use

                try
                {
                    String templateNodeInnerValue = this.TemplateNodeInnerText0;
                    // is it a prolog query or just text ?
                   
                    if (!templateNodeInnerValue.Contains("("))
                    {
                        // just text , so return
                        return templateNodeInnerValue;
                    }
                    //String templateNodeInnerValue = this.TemplateNodeInnerText;
                    //String templateNodeInnerValue = this.templateNode.InnerText;

                   string myMt = TemplateNodeAttributes["mt"].Value;
                   string choice = "random";
                    try
                    {
                         if (TemplateNodeAttributes["choice"] != null)
                            choice = TemplateNodeAttributes["choice"].Value.ToLower();
                    }
                    catch (Exception e) { }

                    string onFail = null;
                    try
                    {
                        if (TemplateNodeAttributes["onfail"] != null)
                            onFail = TemplateNodeAttributes["onfail"].Value;
                    }
                    catch (Exception e) { }

                    string onSuccess = null;
                    try
                    {
                        if (TemplateNodeAttributes["onsuccess"] != null)
                            onSuccess = TemplateNodeAttributes["onsuccess"].Value;
                    }
                    catch (Exception e) { }

                    string query = templateNodeInnerValue;

                    if (myMt == null) { myMt = bot.GlobalSettings.grabSetting("logictextmt"); }

                    query = query.Replace('\n', ' ');
                    query = query.Replace('\r', ' ');
                    query = query.Trim();

                    // iterative deepening
                    int testdepth = 64;
                    List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                    while ((bingingsList.Count == 0) && (testdepth < 256))
                    {
                        testdepth = (int)(testdepth * 1.5);
                        Console.WriteLine("Trying depth {0}", testdepth);
                        //prologEngine.maxdepth = testdepth;
                        bot.servitor.prologEngine.askQuery(query, myMt, out bingingsList);
                    }

                    string finalSay = "";
                    if (bingingsList.Count == 0)
                    {
                        Console.WriteLine("No Logical Solution found");
                    }
                    else
                    {
                        if (choice =="random")
                        {
                            // Pick one at random
                            int randomBinding = rgen.Next(0, bingingsList.Count);
                            Dictionary<string, string> bindings = bingingsList[randomBinding];
                            foreach (string k in bindings.Keys)
                            {
                                string v = bindings[k];
                                //Console.WriteLine("BINDING {0} = {1}", k, v);
                                if (k == "TEXT")
                                {
                                    v = v.Replace("\"", "");
                                    finalSay = v;
                                    Console.WriteLine("ANSMOD = '{1}'", k, v);

                                }
                            }
                        }
                        else
                        {
                            // Concat all the output options (maybe a report)
                            foreach (Dictionary<string, string> bindings in bingingsList)
                            {
                                foreach (string k in bindings.Keys)
                                {
                                    string v = bindings[k];
                                    //Console.WriteLine("BINDING {0} = {1}", k, v);
                                    if (k == "TEXT")
                                    {
                                        v = v.Replace("\"", "");
                                        finalSay += v +"\n";
                                        Console.WriteLine("ANSMOD = '{1}'", k, v);

                                    }
                                }
                            }

                        }

                    }


                    if (finalSay.Length > 1)
                    {
                        logicAns = finalSay;
                        if ((logicAns != null) && (logicAns.Length > 0))
                        {
                            // any answer post processing ?
                        }
                        else
                        {

                            logicAns = "Sorry, I don't understand.";
                            if (onFail != null)
                            {
                                bot.myBehaviors.queueEvent(onFail);
                            }

                        }

                    }
                    else
                    {

                        logicAns = "Sorry, I don't understand.";
                        if (onFail != null)
                        {
                            bot.myBehaviors.queueEvent(onFail);
                        }

                    }
                    bot.logText(String.Format("logictext :" + logicAns));
                    return logicAns;
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: {0} {1}", e.Message, e.StackTrace);
                    logicAns = "Processing caused the following error. " + e.Message;
                    return logicAns;
                }

            }
            return String.Empty;

        }

    }
}
