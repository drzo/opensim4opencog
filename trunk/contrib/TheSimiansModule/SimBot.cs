using cogbot;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace TheSimiansModule
{
    class SimBot : Command
    {
        //BotRegionModel BRM;

        private SimThinker _thinker;

        public SimBot(BotClient Client)
        {
            Name = "simbot";
            Description = "Start theOpenSims type AI.";
            Usage = "simbot [on|start|stop|off|think|needs|info|load]";
        }

        public SimThinker Thinker
        {
            get
            {
                if (_thinker == null)
                {
                    SimActor clientWorldSystemTheSimAvatar = Client.WorldSystem.TheSimAvatar;
                    _thinker = new SimThinker(clientWorldSystemTheSimAvatar);
                }
                return _thinker;
            }
           //set { _thinker = value; }
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (_thinker==null)
            {
                if (Client.Self.AgentID == UUID.Zero) return Failure("waiting for Agent ID");
            }
            if (args.Length > 0)
            {
                if (args[0] == "debug")
                {
                    SimThinker avatar = GetSimAvatar();
                    avatar.ShowDebug();
                    return Success("Debug on " + avatar);
                }

                if (args[0] == "on")
                {
                    SimThinker avatar = GetSimAvatar();
                    return Success("Turned on " + avatar);
                }

                if (args[0] == "start")
                {
                    SimThinker avatar = GetSimAvatar();
                    avatar.StartThinking();
                    return Success("Started Thinking " + avatar);
                }

                if (args[0] == "needs")
                {
                    SimThinker avatar = GetSimAvatar();
                    return Success(avatar.CurrentNeeds.ToString());
                }

                if (args[0] == "think")
                {
                    SimThinker avatar = GetSimAvatar();
                    if (avatar.IsThinking())
                    {
                        avatar.PauseThinking();
                    }
                    avatar.ThinkOnce();
                    return Success("Think once " + avatar);
                }

                if (args[0] == "info")
                {
                    SimThinker avatar = GetSimAvatar();
                    return Success("List " + avatar.DebugInfo());
                }


                //  if (BRM == null) return "the bot is off";

                if (args[0] == "stop")
                {
                    SimThinker avatar = GetSimAvatar();
                    avatar.PauseThinking();
                    return Success("Stopped " + avatar);
                }
                else if (args[0] == "off")
                {
                    //    if (BRM == null) return "the bot was off";
                    SimThinker avatar = GetSimAvatar();
                    avatar.PauseThinking();
                    //   BRM = null;
                    return Success("Stopped " + avatar);
                }

                if (args[0] == "load")
                {
                    SimTypeSystem.LoadConfig(args[1]);
                    WorldSystem.RescanTypes();
                    return Success("(Re)Loaded " + args[1]);
                }
            }
            return Failure(Usage);
        }

        private SimThinker GetSimAvatar()
        {
            return Thinker;

        }

    }
}
