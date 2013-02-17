using System;
using System.Threading;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using AltAIMLbot.Utils;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot;

namespace AltAIMLbot.Web
{
    public class WebScriptExecutor : ScriptExecutorGetter, ScriptExecutor
    {
        #region Implementation of ScriptExecutorGetter

        private AltBot TheBot;
        //private User myUser;

        public WebScriptExecutor(AltBot bot)
        {
            TheBot = bot;
        }
        public ScriptExecutor GetScriptExecuter(object o)
        {
            return this;
        }

        public void WriteLine(string s, params object[] args)
        {
            s = TextPatternUtils.SafeFormat(s, args);
            if (s.StartsWith("Trace")) return;
            if (s.StartsWith("Debug")) return;
            TheBot.writeToLog("HTTPD: " + s);
        }

        #endregion

        #region Implementation of ScriptExecutor

        public Queue<string> responseQueue=new Queue<string>();

        public void webSayResponse(string message)
        {
            Console.WriteLine("SERVITOR Enqueues Web Response:{0}", message);
            responseQueue.Enqueue(message);
        }

        public CmdResult ExecuteCommand(string s, object session, OutputDelegate outputDelegate, CMDFLAGS needResult)
        {
            string verb = GetType().Name;
            StringWriter sw = new StringWriter();
            if (s == null) return ACmdResult.Complete(verb, "null cmd", false);
            s = s.Trim();
            if (s == "") return ACmdResult.Complete(verb, "empty cmd", false);
            if (TheBot.useServitor)
            {
                TheBot.updateRTP2Sevitor();
                string input = s;
                string res = "";
                input = input.Replace("aiml @withuser null -", "");
                input = input.Replace("withuser null", "");
                input = input.Replace("aiml @", "");
                input = input.Replace("- ?", "");
                bool r = true;
                TheBot.servitor.curBot.saySapi = true;
                TheBot.servitor.curBot.sayProcessor = new sayProcessorDelegate(webSayResponse);
                if (input.Length > 1)
                {
                    res = TheBot.servitor.respondToChat(input, TheBot.LastUser);
                    if ((res != null) && (res.Length > 0)) responseQueue.Enqueue(res);
                    res = "";
                    // Give the servitor a chance to do something;
                    int ticks = 0;
                    while ((ticks < 25) && (responseQueue.Count == 0))
                    {
                        Thread.Sleep(200);
                        ticks++;
                    }
                    while (responseQueue.Count > 0)
                    {
                        res += responseQueue.Dequeue() + " ";
                    }
                    if (outputDelegate != null) outputDelegate(res);
                    WriteLine(res);
                    TheBot.updateServitor2RTP();
                }
                else
                {
                    res = "";
                    // Give the servitor a chance to do something;
                    int ticks = 0;
                    while ((ticks < 3) && (responseQueue.Count == 0))
                    {
                        Thread.Sleep(200);
                        ticks++;
                    }
                    while (responseQueue.Count > 0)
                    {
                        res += responseQueue.Dequeue() + " ";
                    }
                    if (outputDelegate != null) outputDelegate(res);
                    WriteLine(res);
                }
                return ACmdResult.Complete(verb, res, r);

            }
            else
            {
                if (s.StartsWith("aiml"))
                {
                    s = s.Substring(4).Trim();
                    if (s.StartsWith("@ "))
                        s = "@withuser" + s.Substring(1);
                }
                if (!s.StartsWith("@")) s = "@" + s;
                //     sw.WriteLine("AIMLTRACE " + s);
                User myUser = null;// TheBot.LastUser;
                //OutputDelegate del = outputDelegate ?? sw.WriteLine;
                bool r = TheBot.BotDirective(myUser, s, sw.WriteLine);
                sw.Flush();
                string res = sw.ToString();
                // for now legacy
                //res = res.Replace("menevalue=", "mene value=");
            if (outputDelegate != null) outputDelegate(res);
            WriteLine(res);
            return ACmdResult.Complete(verb, res, r);
            }
        }

        public CmdResult ExecuteXmlCommand(string s, object session, OutputDelegate outputDelegate)
        {
            return ExecuteCommand(s, session, outputDelegate, CMDFLAGS.Foregrounded);
        }

        public string GetName()
        {
            return TheBot.GlobalSettings.grabSetting("NAME");
        }

        public object getPosterBoard(object slot)
        {
            string sslot = "" + slot;
            sslot = sslot.ToLower();
            var u = TheBot.GlobalSettings.grabSetting(sslot);
            if (Unifiable.IsNull(u)) return null;
            if (TextPatternUtils.IsNullOrEmpty(u)) return "";
            return u;//.ToValue(null);
        }

        #endregion
    }
}