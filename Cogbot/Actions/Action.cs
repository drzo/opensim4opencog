using System;
using OpenMetaverse;
using System.IO;


namespace cogbot.Actions
{
    /// <summary>
    /// An interface for commands is only invoked on Region mastering bots
    /// Such as terrain uploads and simulator info (10 bots doing the command at once will create problems)
    /// Non region master bots are thinner clients and usually not fit for object tracking
    /// </summary>
    public interface RegionMasterCommand
    {
    }
    /// <summary>
    /// An interface for commands that do not target any specific bots
    ///  Such as pathsystem maintainance or application commands
    /// </summary>
    public interface SystemApplicationCommand
    {
    }

    /// <summary>
    /// An interface for commands that do not require a connected grid client
    /// such as Login or settings but still targets each bot individually
    /// </summary>
    public interface BotSystemCommand
    {
    }

    abstract public class Action
    {
        /// <summary>
        /// When set to true, think will be called.
        /// </summary>
        public bool Active;

        /// <summary>
        /// Called twice per second, when Command.Active is set to true.
        /// </summary>
        public virtual void Think()
        {
        }
        public UUID UUIDParse(string p)
        {
            UUID uuid;
            if (UUIDTryParse(p, out uuid)) return uuid;
            return UUID.Parse(p);
        }

        public bool UUIDTryParse(string[]p, int start, out UUID target)
        {
            return UUIDTryParse(String.Join(" ",p,start,p.Length-start),out target);
        }
        public bool UUIDTryParse(string p, out UUID target)
        {
            if (p.Contains("-") && UUID.TryParse(p, out target)) return true;
            if (p.Contains("-") && UUID.TryParse(p.Split(new char[] { ' ' })[0], out target)) return true;
            Primitive prim;
            if (WorldSystem.tryGetPrim(p,out prim)) {
                target = prim.ID;
                if (target != UUID.Zero) return true;
            }
            target = WorldSystem.GetAnimationUUID(p);
            if (target != UUID.Zero) return true;
            return false;
        }


        public BotClient m_Client = null;

        public BotClient Client
        {
            get
            {
                return TheBotClient;
            }
            set
            {
                TheBotClient = value;
            }
        }


        public BotClient TheBotClient
        {
            get
            {
                if (m_Client != null) return m_Client;
                return cogbot.Listeners.WorldObjects.Master.client;
            }
            set
            {
                m_Client = value;
            }
        }

        public static TextForm ClientManager
        {
            get
            {
                return TextForm.SingleInstance;
            }
        }

        public cogbot.Listeners.WorldObjects WorldSystem
        {
            get
            {
                if (m_Client == null) return cogbot.Listeners.WorldObjects.Master;
                return m_Client.WorldSystem;
            }
        }

        public string Name;
        protected string helpString;
        protected string usageString;

        public Action(BotClient _parent)
        {
            helpString = "No help information for this action.";
            usageString = "No usage instruction for this action.";

           // Client = _parent;
            m_Client = _parent;//.CurrentClient;
        }

        readonly protected System.Text.StringBuilder writeBuffer = new System.Text.StringBuilder();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="format"></param>
        /// <param name="arg"></param>
        public void WriteLine(string format, params object[] arg)
        {

            String s;
            if (arg == null || arg.Length == 0) { s = format; }
            else
            {
                s = String.Format(format, arg);
            }
            Console.WriteLine(s);
            lock (writeBuffer)
            {
                writeBuffer.AppendLine(s);
            }
            if (TheBotClient != null) TheBotClient.output(s);
        } // method: WriteLine

        public string acceptInputWrapper(string verb, string args)
        {
           return acceptInput(verb, new Parser(args));
        }

        public string ExecuteBuffer(string[] args, UUID fromAgentID)
        {
            lock (writeBuffer)
            {
                writeBuffer.Remove(0, writeBuffer.Length);
                Parser p = new Parser(String.Join(" ", args));
                p.tokens = args;
                string str = Execute(args, fromAgentID);
                string ret = writeBuffer.ToString();
                writeBuffer.Remove(0, writeBuffer.Length);
                return (ret + "\n" + str).Trim();
            }
        }

        public virtual string Execute(string[] args, UUID fromAgentID)
        {
            Parser p = new Parser(String.Join(" ", args));
            p.tokens = args;
            return acceptInput(Name, p);
        }

        public abstract string acceptInput(string verb, Parser args);

        public virtual string makeHelpString()
        {
            return helpString;
        }
        public virtual string makeUsageString()
        {
            return usageString;
        }

        public virtual string GetDescription()
        {
            return helpString + "  Usage: " + usageString;
        }
    }
}
