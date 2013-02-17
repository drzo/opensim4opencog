using System;
using System.Collections;
using System.Reflection;
#if (COGBOT_LIBOMV || USE_STHREADS)
using AltAIMLParser;
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;

using Cogbot;
using Cogbot.Actions;
using Cogbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot;
using Cogbot.World;
using OpenMetaverse;
using AIMLbot;

namespace AltAIMLParser
{
    
}

namespace AIMLBotModule
{
    public class AimlCommand : Cogbot.Actions.Command, BotStatefullCommand, BotSystemCommand
    {
        public static string UNKNOWN_PARTENER = "null";
        private string lastKnownUser;
        public AimlCommand(BotClient testClient)
            : base(testClient)
        {
            Name = "aiml";
            TheBotClient = testClient;

        }



        override public void MakeInfo()

        {

			            Description = "Usage: aiml [@[[on|off|reload|learn]|text|setuser]] [operands]";
            Category = CommandCategory.Communication;
        }

        public WorldObjectsForAimLBot WorldSystemModule
        {
            get
            {
                String mn = "AIMLBotModule";
                Listener wmab;
                var dict = Client.Plugins;
                while (true)
                {
                    lock (dict)
                        if (dict.TryGetValue(mn, out wmab))
                        {
                            var v = (WorldObjectsForAimLBot)wmab;
                            if (v.MyBot != null) return v;
                        }
                    Thread.Sleep(3000);
                    DLRConsole.DebugWriteLine("waiting on " + mn);
                }
            }
        }
        public bool IsBotLoaded
        {
            get
            {
                String mn = "AIMLBotModule";
                Listener wmab;
                var dict = Client.Plugins;
                lock (dict)
                    return (dict.TryGetValue(mn, out wmab));
            }
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            try
            {
                var fromAgentID = CogbotHelpers.NonZero((UUID)BotClient.SessionToCallerId(args.CallerAgent), UUID.Zero);
                return Execute0(args, fromAgentID, WriteLine);
            }
            catch (Exception e)
            {
                return Failure("" + e);
            }
        }

        private CmdResult Execute0(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string defaultAIMLUser;
            if (args.Length == 0) return Failure(Details);
            string s = args[0].ToLower();
            string joined = String.Join(" ", args);
            bool legacyCmd = " on off help reload ".Contains(" " + s + " ");
            if (s == "@user" || s == "@chuser")
            {
                defaultAIMLUser = String.Join(" ", args, 1, args.Length - 1);

                int lastIndex = defaultAIMLUser.IndexOf("-");
                if (lastIndex > 0)
                {
                    string oldUser = defaultAIMLUser.Substring(lastIndex + 1).Trim();
                    defaultAIMLUser = defaultAIMLUser.Substring(0, lastIndex - 1);
                }
                if (!IsBotLoaded) return Failure("AIML Module Not ready yet");
                SetUser(defaultAIMLUser);
                if (s == "@chuser")
                {
                    bool res = WorldSystemModule.DoBotDirective(args, fromAgentID, WriteLine);
                    if (!res)
                    {
                        Failure("ERROR from AIML " + joined);
                    }
                    return Success("REAL WORLD returned: " + joined);
                }
                return Success("WorldObjects.SetDefaultUser = " + defaultAIMLUser + ";");
            }
            if (s == "@")
            {
                joined = joined.TrimStart(new[] {'@', ' '});
                int lastIndex = joined.IndexOf("-");
                defaultAIMLUser = joined.Substring(0, lastIndex).Trim();
                s = "-";
                SetUser(defaultAIMLUser);
            }
            else if (s.StartsWith("@") || (args.Length == 1 && legacyCmd))
            {
                try
                {
                    if (!IsBotLoaded) return Failure("AIML Module Not ready yet");
                    if (legacyCmd) args[0] = "@" + s;
                    bool res = WorldSystemModule.DoBotDirective(args, fromAgentID, WriteLine);
                    if (!res)
                    {
                        Failure("WARN from AIML " + joined);
                    }
                    return Success("aiml returned: " + joined);
                }
                catch (Exception e)
                {
                    return Failure("" + e);
                }
            }
            if (s == "-")
            {
                int lastIndex = joined.IndexOf("-");
                joined = joined.Substring(lastIndex + 1).Trim();
            }

            double ratng;
            if (!IsBotLoaded) return Failure("AIML Module Not ready yet");
            var MyBot = WorldSystemModule.MyBot;
            var myUser = MyBot.FindOrCreateUser(lastKnownUser);
            //lock (myUser.QueryLock)
            {
                myUser.CurrentRequest = null;
                String useOut = WorldSystemModule.AIMLInterpScored(joined, myUser, out ratng, RequestKind.ChatForString);
                double scored = ratng;
                WorldSystemModule.MyBot.writeToLog("REALWORLD AIMLTRACE! '" + joined + "' " + scored + " '" + useOut +
                                                   "'");
                if (String.IsNullOrEmpty(useOut)) useOut = "Interesting.";
                if (!useOut.Contains("menevalue="))
                    useOut = string.Format("{0} (menevalue= {1:1} )", useOut.Replace(" _", " "), (int) scored);
                return Success(useOut);
            }
        }

        public void SetUser(string user)
        {
            string setUser;
            DLRConsole.DebugWriteLine("<- SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            if (AltBot.UnknowableName(user))
            {
                if (AltBot.UnknowableName(lastKnownUser))
                {
                    DLRConsole.DebugWriteLine("THEREFORE Same persom with still unknown name (make one up)");
                    setUser = lastKnownUser = UNKNOWN_PARTENER;
                }
                else
                {
                    DLRConsole.DebugWriteLine("THEREFORE New Person with unknown name");
                    setUser = lastKnownUser = user;
                }
            }
            else
            {
                if (AltBot.UnknowableName(lastKnownUser))
                {
                    DLRConsole.DebugWriteLine("THEREFORE Same Person now known name");
                    WorldSystemModule.RenameUser(lastKnownUser, user);
                    setUser = lastKnownUser = user;
                }
                else
                {
                    if (WorldSystemModule.SameUser(lastKnownUser, user))
                    {
                        DLRConsole.DebugWriteLine("THEREFORE Different Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                    else
                    {
                        DLRConsole.DebugWriteLine("THEREFORE New Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                }
            }
            user = setUser;
            DLRConsole.DebugWriteLine("-> SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            WorldSystemModule.SetDefaultUser(user);
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
           
        }

        #endregion
    }

    public class AIMLEventSubscriber : SimEventSubscriber
    {
        private AltBot AimlBot;
        private readonly WorldObjectsModule World;
        public AIMLEventSubscriber(AltBot bot, WorldObjectsModule obj)
        {
            AimlBot = bot;
            World = obj;
        }

        #region SimEventSubscriber Members

        public void OnEvent(CogbotEvent evt)
        {
            String s = evt.Verb.ToLower();
            if (s.StartsWith("on-chat"))
            {
                return;
            }
            string aimlCall = string.Format("SimEvent {0} {1}", evt.Verb, argsListString(evt.GetArgs()));
            //DLRConsole.DebugWriteLine(aimlCall);
            //Result r = AimlBot.Chat(aimlCall,"EventSystem");
        }

        public string argsListString(IEnumerable args)
        {
            if (args == null) return "NiL";
            IEnumerator enumer = args.GetEnumerator();
            if (enumer == null) return "niL";
            if (!enumer.MoveNext()) return "";
            String msg = argString(enumer.Current);
            while (enumer.MoveNext())
            {
                msg += " ";
                msg += argString(enumer.Current);
            }
            return msg;

        }

        public string argString(object arg)
        {
            if (arg == null) return "null";
            if (arg is String)
            {
                return arg.ToString().Replace("  ", " ");
            }
            Type type = arg.GetType();
            if (arg is Simulator)
            {
                return argString(((Simulator)arg).Name);
            }
            if (arg is Avatar)
            {
                Avatar prim = (Avatar)arg;
                return prim.Name;
            }

            if (arg is Primitive)
            {
                Primitive prim = (Primitive)arg;
                arg = World.GetSimObject(prim);
                //if (prim.Properties != null)
                //{
                //    arg = arg + " " + argString(prim.Properties.Name);
                //}
                //return arg + ")";
            }
            if (arg is SimObject)
            {
                return arg.ToString();
            }
            if (type.IsEnum)
            {
                return argString(arg.ToString());
            }
            //InternalDictionary
            if (arg is IList)
            {
                String dictname = "'(list " + type.Name;
                IList list = (IList)arg;
                foreach (object key in list)
                {
                    dictname += " " + argString(key);
                }
                return dictname + ")";


            }

            if (arg is Parcel)
            {
                String dictname = "'(parcel";
                Parcel list = (Parcel)arg;
                dictname += " " + argString(list.SnapshotID.ToString());
                dictname += " " + argString(list.Name);
                return dictname + ")";
            }
            if (arg is Group)
            {
                Group list = (Group)arg;
                return argString(list.Name);
            }
            if (arg is IDictionary)
            {
                String dictname = "'(dict " + type.Name;
                IDictionary dict0 = (IDictionary)arg;
                IDictionary dict = dict0;
                lock (dict.SyncRoot)
                {
                    foreach (object key in dict.Keys)
                    {
                        Object o = dict[key];
                        dictname += " " + argString(key) + "=" + argString(o);
                    }
                    return dictname + ")";
                }

            }

            //if (arg is Quaternion)
            //{
            //    Quaternion quat = (Quaternion)arg;
            //    quat.Normalize();
            //    arg = WorldSystem.QuatToRotation(quat);
            //}

            if (arg is Quaternion)
            {
                Quaternion vect = (Quaternion)arg;
                return "'(Quaternion " + vect.X + " " + vect.Y + " " + vect.Z + " " + vect.W + ")";
            }

            if (arg is UUID)
            {
                //   if (true) return argString(arg.ToString());
                object found = WorldObjects.GridMaster.GetObject((UUID)arg);
                if (found == null || found is UUID)
                {
                    return argString(arg.ToString());
                }
                return argString(found);
            }
            else
                if (arg is Vector3)
                {
                    Vector3 vect = (Vector3)arg;
                    return "'(Vector3 " + vect.X + " " + vect.Y + " " + vect.Z + ")";
                }
                else
                    if (arg is Vector2)
                    {
                        Vector2 vect = (Vector2)arg;
                        return "'(Vector2 " + vect.X + " " + vect.Y + ")";
                    }
                    else
                        if (arg is Vector3d)
                        {
                            Vector3d vect = (Vector3d)arg;
                            return "'(Vector3d " + vect.X + " " + vect.Y + " " + vect.Z + ")";
                        }

            if (type.IsArray)
            {
                Array a = (Array)arg;
                return "#{/*" + type + "*/" + argsListString(a) + "}";
            }

            if (type.Namespace.StartsWith("System"))
            {
                return "" + arg;
            }
            if (arg is IEnumerable)
            {
                IEnumerable a = (IEnumerable)arg;
                return "'(/*" + type + "*/" + argsListString(a) + ")";
            }
            if (type.IsValueType)
            {
                String tostr = "{" + arg + "";
                foreach (FieldInfo fi in type.GetFields())
                {
                    if (!fi.IsStatic)
                    {
                        tostr += ",";
                        tostr += fi.Name + "=";
                        tostr += argString(fi.GetValue(arg));
                    }
                }
                return argString(tostr + "}");
            }
            if (!type.IsValueType)
            {
                String tostr = "{" + arg + "";
                foreach (FieldInfo fi in type.GetFields())
                {
                    if (!fi.IsStatic)
                    {
                        tostr += ",";
                        tostr += fi.Name + "=";
                        tostr += fi.GetValue(arg);
                    }
                }
                return argString(tostr + "}");
            }
            return "" + arg;
        }

        public void Dispose()
        {
            //throw new NotImplementedException();
        }

        public bool EventsEnabled
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        #endregion
    }
}