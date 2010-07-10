using System;
using System.Collections;
using System.Reflection;
using System.Threading;
using cogbot;
using cogbot.Actions;
using cogbot.Listeners;
using RTParser;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace AIMLBotModule
{
    public class AimlCommand : cogbot.Actions.Command, BotPersonalCommand
    {
        public static string UNKNOWN_PARTENER = "UNKNOWN_PARTENR";
        private string lastKnownUser;
        public AimlCommand(BotClient testClient)
        {
            Name = "aiml";
            Description = "Usage: aiml [[on|off|reload|learn]|text|user]";
            Category = CommandCategory.Communication;
        }

        public WorldObjectsForAimLBot WorldSystemModule
        {
            get
            {
                String mn = "AIMLBotModule";
                Listener wmab;
                var dict = Client.listeners;
                while (true)
                {
                    lock (dict)
                        if (dict.TryGetValue(mn, out wmab))
                        {
                            var v = (WorldObjectsForAimLBot)wmab;
                            if (v.MyBot != null) return v;
                        }
                    Thread.Sleep(3000);
                    Console.WriteLine("waiting on " + mn);
                }
            }
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            try
            {
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
            if (args.Length == 0) return Failure(Usage);
            string s = args[0].ToLower();
            string joined = String.Join(" ", args);
            bool legacyCmd = " on off help reload ".Contains(" " + s + " ");
            if (s == "@user")
            {
                defaultAIMLUser = String.Join(" ", args, 1, args.Length - 1);
                SetUser(defaultAIMLUser);
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
            String ss = WorldSystemModule.AIMLInterp(joined, lastKnownUser);
            if (String.IsNullOrEmpty(ss)) ss = "Interesting.";
            return Success(ss);
        }

        public void SetUser(string user)
        {
            string setUser;
            Console.WriteLine("<- SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            if (RTPBot.UnknowableName(user))
            {
                if (RTPBot.UnknowableName(lastKnownUser))
                {
                    Console.WriteLine("THEREFORE Same persom with still unknown name (make one up)");
                    setUser = lastKnownUser = UNKNOWN_PARTENER;
                }
                else
                {
                    Console.WriteLine("THEREFORE New Person with unknown name");
                    setUser = lastKnownUser = user;
                }
            }
            else
            {
                if (RTPBot.UnknowableName(lastKnownUser))
                {
                    Console.WriteLine("THEREFORE Same Person now known name");
                    WorldSystemModule.RenameUser(lastKnownUser, user);
                    setUser = lastKnownUser = user;
                }
                else
                {
                    if (WorldSystemModule.SameUser(lastKnownUser, user))
                    {
                        Console.WriteLine("THEREFORE Different Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                    else
                    {
                        Console.WriteLine("THEREFORE New Person with known name");
                        setUser = user;
                        lastKnownUser = user;
                    }
                }
            }
            user = setUser;
            Console.WriteLine("-> SetUser=" + user + " lastKnownUser=" + lastKnownUser);
            WorldSystemModule.SetDefaultUser(user);
        }
    }

    public class AIMLEventSubscriber : SimEventSubscriber
    {
        private RTPBot AimlBot;
        private readonly WorldObjectsModule World;
        public AIMLEventSubscriber(RTPBot bot, WorldObjectsModule obj)
        {
            AimlBot = bot;
            World = obj;
        }

        #region SimEventSubscriber Members

        public void OnEvent(SimObjectEvent evt)
        {
            String s = evt.GetVerb().ToLower();
            if (s.StartsWith("on-chat"))
            {
                return;
            }
            string aimlCall = string.Format("SimEvent {0} {1}", evt.GetVerb(), argsListString(evt.GetArgs()));
            //Console.WriteLine(aimlCall);
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

        #endregion
    }
}