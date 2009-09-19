using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using Radegast;

namespace cogbot.Actions
{
    public enum CommandCategory : int
    {
        Parcel,
        Appearance,
        Movement,
        Simulator,
        Communication,
        Inventory,
        Objects,
        Voice,
        TestClient,
        Friends,
        Groups,
        Other,
        Unknown
    }
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

    public abstract class Command : IComparable
    {
        protected OutputDelegate WriteLine;


        public Command()
            : this(null)
        {
            
        } // constructor

        public Command(BotClient bc)
        {
            Name = GetType().Name.Replace("Command", "");
            _mClient = bc;
        } // constructor



        /// <summary>
        /// 
        /// </summary>
        /// <param name="verb"></param>
        /// <param name="args"></param>
        public virtual string acceptInput(string verb, cogbot.Actions.Parser args, OutputDelegate WriteLine)
        {
            this.WriteLine = WriteLine;
            return Execute(args.tokens, UUID.Zero, WriteLine);
        } // method: acceptInput


        public string Description
        {
            get
            {
                return GetDescription();
            }
            set
            {
                if (String.IsNullOrEmpty(value)) return;
                int half = value.ToLower().IndexOf("usage");
                if (half == -1)
                {
                    half = value.ToLower().IndexOf("use");
                }
                if (half==-1)
                {
                    helpString = value;
                    return;
                }
                Description = value.Substring(0, half).TrimEnd();
                Usage = value.Substring(half);

            }
        }

        public string Usage
        {
            get { return makeUsageString(); }
            set
            {
                usageString = value.Trim().Replace("Usage:", " ").Replace("usage:", " ").Replace("Use:", " ").Trim();
            }
        }

        public CommandCategory Category;
        /// <summary>
        /// When set to true, think will be called.
        /// </summary>
        public bool Active;
        public string Name { get; set; }
        protected string helpString;
        protected string usageString;
        /// <summary>
        /// Introspective Parameters for calling command from code
        /// </summary>
        public NamedParam[] Parameters;
        /// <summary>
        /// Called twice per second, when Command.Active is set to true.
        /// </summary>
        public virtual void Think()
        {
        }

        private BotClient _mClient = null;

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
                if (_mClient != null) return _mClient;
                return cogbot.Listeners.WorldObjects.GridMaster.client;
            }
            set
            {
                _mClient = value;
            }
        }

        public static ClientManager ClientManager
        {
            get
            {
                return ClientManager.SingleInstance;
            }
        }

        public cogbot.Listeners.WorldObjects WorldSystem
        {
            get
            {
                if (_mClient == null) return cogbot.Listeners.WorldObjects.GridMaster;
                return _mClient.WorldSystem;
            }
        }

        public SimActor TheSimAvatar
        {
            get
            {
                return WorldSystem.TheSimAvatar;
            }
        }

        public string acceptInputWrapper(string verb, string args, OutputDelegate WriteLine)
        {
            this.WriteLine = WriteLine;
            return acceptInput(verb, Parser.ParseArgs(args), WriteLine);
        }

        public virtual string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            this.WriteLine = WriteLine;
            Parser p = Parser.ParseArgs(String.Join(" ", args));
            p.tokens = args;
            return acceptInput(Name, p, WriteLine);
        }

        public int CompareTo(object obj)
        {
            if (obj is Command)
            {
                Command c2 = (Command)obj;
                return Category.CompareTo(c2.Category);
            }
            else
                throw new ArgumentException("Object is not of type Command.");
        }
        public virtual string GetDescription()
        {
            if (!string.IsNullOrEmpty(helpString)) return helpString;
            return helpString + "  Usage: " + Usage;
        }


        public virtual string makeHelpString()
        {
            if (!string.IsNullOrEmpty(helpString)) return helpString;
            return helpString;
        }

        public virtual string makeUsageString()
        {
            if (!String.IsNullOrEmpty(usageString)) return usageString;
            return helpString;
        }
                
        // Helpers

        protected Vector3 GetSimPosition()
        {
            return TheSimAvatar.GetSimPosition();
        }

        public UUID UUIDParse(string p)
        {
            UUID uuid;
            if (UUIDTryParse(p, out uuid)) return uuid;
            return UUID.Parse(p);
        }

        public bool UUIDTryParse(string[] p, int start, out UUID target)
        {
            return UUIDTryParse(String.Join(" ", p, start, p.Length - start), out target);
        }
        public bool UUIDTryParse(string p, out UUID target)
        {
            if (p.Contains("-") && UUID.TryParse(p, out target)) return true;
            if (p.Contains("-") && UUID.TryParse(p.Split(new char[] { ' ' })[0], out target)) return true;
            Primitive prim;
            if (WorldSystem.tryGetPrim(p, out prim))
            {
                if (prim != null)
                {
                    target = prim.ID;
                    if (target != UUID.Zero) return true;
                }
            }
            target = WorldSystem.GetAssetUUID(p, AssetType.Unknown);
            if (target != UUID.Zero) return true;
            return false;
        }
    }
}
