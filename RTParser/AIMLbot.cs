using System;
using System.Collections.Generic;
using System.Text;
using RTParser;
using RTParser.Utils;

namespace AIMLbot
{
    public class Bot : RTParser.RTPBot
    {
         
        public Bot()
            : base()
        {
        }
        public void loadAIMLFromFiles()
        {
            base.loadAIMLFromDefaults();
        }
    }
    public class MasterUser : RTParser.User
    {
        public MasterUser(string UserID, Bot bot)
            : base(UserID, bot)
        {
        }
        public MasterUser(string UserID, RTPBot bot)
            : base(UserID, bot)
        {
        }
    }
    public class MasterRequest : MasterResult
    {/*
        public Request(String rawInput, RTParser.User user, RTPBot bot)
            : this(rawInput, user, bot, null)
        {
        }
      
              public MasterRequest(String rawInput, RTParser.User user, RTPBot bot, RTParser.Request r)
            : base(rawInput, user, bot, r, null)
        {
        }
*/
        public MasterRequest(string rawInput, User user, RTPBot bot, Request parent, User targetUser)
            : base(rawInput, user, bot, parent, targetUser)
        {
        }
    }

    abstract public class MasterResult : RTParser.ResultImpl
    {
        public MasterResult(string rawInput, User user, RTPBot bot, Request parent, User targetUser)
            : base(rawInput, user, bot, parent, targetUser)
        {
            if (ParentRequest==null && !IsToplevelRequest)
            {
                writeToLog("orphan?!" + this);
            }
        }
    }
    
    namespace Utils
    {
        public class AIMLLoader : RTParser.Utils.AIMLLoader
        {
            public AIMLLoader(RTPBot bot)
                : base(bot, bot == null ? null : bot.GetBotRequest("-AIMLLoader-"))
            {
            }
        }
    }

}
