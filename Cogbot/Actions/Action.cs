using System;
using OpenMetaverse;
using System.IO;
using cogbot.TheOpenSims;


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
    /// An interface for commands is only invoked on Grid mastering bots
    /// Such as Directory info requests (10 bots doing the command at once will create problems)   
    /// </summary>
    public interface GridMasterCommand
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
                return cogbot.Listeners.WorldObjects.GridMaster.client;
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
                if (m_Client == null) return cogbot.Listeners.WorldObjects.GridMaster;
                return m_Client.WorldSystem;
            }
        }

        public SimActor TheSimAvatar
        {
            get
            {
                return WorldSystem.TheSimAvatar;
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
            Client.WriteLine(s);
        }

        public string acceptInputWrapper(string verb, string args , OutputDelegate WriteLine)
        {

                return acceptInput(verb, Parser.ParseArgs(args), WriteLine);
        }

        public virtual string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            Parser p = Parser.ParseArgs(String.Join(" ", args));
            p.tokens = args;
            return acceptInput(Name, p, WriteLine);
        }

        public abstract string acceptInput(string verb, Parser args, OutputDelegate WriteLine);

        public virtual string makeHelpString()
        {
            return helpString;
        }
        public virtual string makeUsageString()
        {
            return usageString;
        }
        protected Vector3 GetSimPosition()
        {
            return TheSimAvatar.GetSimPosition();
        }
        public virtual string GetDescription()
        {
            return helpString + "  Usage: " + usageString;
        }
    }
}
