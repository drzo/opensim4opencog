using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;


namespace cogbot.Actions
{
    public interface BotMasterCommand
    {
    }
    public interface BotSystemCommand
    {
    }
    public interface BotGridClientCommand
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
            if (p.Contains("-") && UUID.TryParse(p,out target)) return true;
            Primitive prim;
            if (WorldSystem.tryGetPrim(p,out prim)) {
                target = prim.ID;
                if (target != UUID.Zero) return true;
            }
            target = cogbot.TheOpenSims.SimAnimation.GetAnimationUUID(p);
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

        /// <summary>
        /// 
        /// </summary>
        /// <param name="format"></param>
        /// <param name="arg"></param>
        public void WriteLine(string format, params object[] arg)
        {

            String s;
            if (arg == null || arg.Length == 0) s = format;
            else s
                = String.Format(format, arg);
            Console.WriteLine(s);
            if (TheBotClient != null) TheBotClient.output(s);
        } // method: WriteLine

        public void acceptInputWrapper(string verb, string args)
        {
            acceptInput(verb, new Parser(args));
        }

        public virtual string Execute(string[] args, UUID fromAgentID)
        {
            Parser p = new Parser(String.Join(" ", args));
            p.tokens = args;
            acceptInput(Name, p);
            return String.Empty;
        }

        public abstract void acceptInput(string verb, Parser args);

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
