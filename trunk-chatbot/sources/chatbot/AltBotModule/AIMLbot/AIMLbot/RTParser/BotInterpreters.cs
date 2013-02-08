using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using System.Web;
using System.Xml;
using AIMLbot;
using AltAIMLbot;
using AltAIMLParser;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
#if USE_SWIPROLOG
using PrologScriptEngine;
#endif
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using Console=System.Console;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;

namespace RTParser
{

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary
    public partial class AltBot : ScriptInterpreterFactory
    {

        private object EvalAIMLHandler(string cmd, Request user)
        {
            XmlNode node = StaticAIMLUtils.getTemplateNode(cmd);
            LineInfoElementImpl.unsetReadonly(node);
            if (Loader == null)
            {
                Loader = new AIMLLoaderU(this, GetBotRequest("EvalAIMLHandler " + cmd));
            }
            var res = ImmediateAiml(node, user, Loader, RequestKind.EvalAIMLHandler);
            return res;
        }

        internal Unifiable SystemExecute(Unifiable cmd, Unifiable langu, Request user)
        {
            if (IsNullOrEmpty(langu))
            {
                langu = GlobalSettings.grabSetting("systemlang") ?? "bot";
            }
            else
            {
                langu = ToLower(Trim(langu));
            }
            Unifiable s = "The system tag should be doing '" + cmd + "' lang=" + langu;
            writeToLog(s.AsString());
            SystemExecHandler handler;
            if (SettingsDictionaryReal.TryGetValue(ExecuteHandlers, langu, out handler))
            {
                try
                {
                    object o = handler(cmd, user);
                    return Unifiable.Create(o);
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    return Unifiable.Empty;
                }
            }
            else
            {
                try
                {
                    object self = user;
                    ScriptInterpreter parent = null;
                    ScriptInterpreter si = ScriptManager.LoadScriptInterpreter(langu, self, parent);
                    object o = ScriptManager.EvalScriptInterpreter(cmd.ToValue(user.CurrentQuery), langu, self, parent, writeToLog);
                    string siStr = si.Str(o);
                    return Unifiable.Create(siStr);
                }
                catch (Exception e)
                {
                    writeToLog(e);
                }
            }
            writeToLog(s);
            return Unifiable.Empty;
        }

        private void ExternalIntern(string name, object value)
        {
            if (_objr == null)
            {
                lock (PostObjectRequesterSet)
                    PostObjectRequesterSet.Add(() => ScriptManager.AddSetting(ObjectRequester, BotID, name, value));
                return;
            }
            ScriptManager.AddSetting(ObjectRequester, BotID, name, value);
        }

        private AIMLScriptIntperForFiletype AIMLScriptIntperForFiletypeNone = null;
        [ThreadStatic]
        public static AimlResult tl_aimlResult;
        public class AimlResult
        {
            private List<AimlSubResult> resultList = new List<AimlSubResult>();

            public void AddResult(XmlNode node, string filename, object result)
            {
                resultList.Add(new AimlSubResult(node, filename, result));
            }
            public override string ToString()
            {
                if (resultList.Count == 0) return "no results";
                string res = "";
                foreach (var node in resultList)
                {
                    res += "" + node.Result;
                }
                if (string.IsNullOrEmpty(res)) return "blankresults:" + resultList.Count;
                return res;
            }
        }
        public class AimlSubResult
        {
            public XmlNode Node;
            public string Filename;
            public object Result;

            public AimlSubResult(XmlNode node, string filename, object result)
            {
                this.Node = node;
                this.Filename = filename;
                this.Result = result;
            }
        }

        private void SetupExecHandlers()
        {
            AIMLScriptIntperForFiletypeNone = new AIMLScriptIntperForFiletype("aiml", this);
            ScriptManager.AddInterpreter(AIMLScriptIntperForFiletypeNone);
            ExternalIntern("MyBot", this);
            ExternalIntern("BotUsers", BotUsers);
            listeners["AIMLBotModule"] = this;
        }

        #region Implementation of ScriptInterpreterFactory

        public ScriptInterpreter GetLoaderOfFiletype(string type)
        {
            lock (ExecuteHandlers)
            {
                SystemExecHandler handler;
                if (ExecuteHandlers.TryGetValue(type, out handler)) return null;
            }
            return new AIMLScriptIntperForFiletype(type, this);
        }

        #endregion
    }

    public class AIMLScriptIntperForFiletype : ScriptInterpreter
    {
        public bool IsSelf(object was)
        {
            return Self == was;
        }
        public void Init(object was)
        {
            Self = was;
        }

        private AltBot TheBot;
        private string Lang;

        public AIMLScriptIntperForFiletype(string type, AltBot bot)
        {
            this.Lang = type;
            TheBot = bot;
        }

        #region Implementation of ScriptInterpreterFactory

        public ScriptInterpreter GetLoaderOfFiletype(string type)
        {
            return TheBot.GetLoaderOfFiletype(type);
        }

        #endregion

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
        }

        #endregion

        #region Implementation of ScriptInterpreter

        public bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            throw new NotImplementedException();
        }

        public bool LoadsFileType(string filenameorext)
        {
            return filenameorext.EndsWith(Lang);
            var ExecuteHandlers = TheBot.ExecuteHandlers;
            lock (ExecuteHandlers)
            {
                SystemExecHandler handler;
                if (ExecuteHandlers.TryGetValue(filenameorext, out handler)) return true;
                return false;
            }
        }

        public object Read(string context_name, TextReader stringCodeReader, OutputDelegate WriteLine)
        {
            throw new NotImplementedException();
        }

        public bool Eof(object codeTree)
        {
            throw new NotImplementedException();
        }

        public void Intern(string varname, object value)
        {
            throw new NotImplementedException();
        }

        public object Eval(object code)
        {
            throw new NotImplementedException();
        }

        public object ConvertArgToLisp(object code)
        {
            throw new NotImplementedException();
        }

        public object ConvertArgFromLisp(object code)
        {
            throw new NotImplementedException();
        }

        public string Str(object code)
        {
            throw new NotImplementedException();
        }

        public ScriptInterpreter newInterpreter(object self)
        {
            throw new NotImplementedException();
        }

        public bool IsSubscriberOf(string eventName)
        {
            throw new NotImplementedException();
        }

        public object GetSymbol(string eventName)
        {
            throw new NotImplementedException();
        }

        public void InternType(Type t)
        {
            if (!typeof (AIMLTagHandlerU).IsAssignableFrom(t)) return;
            TagHandlerProcessor.AddTagHandler(t);
        }

        public object Self
        {
            get { return TheBot.ObjectRequester; }
            set
            {
                if (value != TheBot.ObjectRequester)
                {
                    throw new NotSupportedException("not a robnot");
                }
            }
        }

        public object Impl
        {
            get { return TheBot; }
        }

        #endregion
    }
}
