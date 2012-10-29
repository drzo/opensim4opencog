using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLParser;
using RTParser;
using RTParser.Utils;
using Result=AltAIMLbot.Result;
using User=AltAIMLbot.User;

namespace AIMLbot
{
    public class Bot : RTParser.AltBot
    {
         
        public Bot()
            : base()
        {
            //useServitor = true;
        }
        public void loadAIMLFromFiles()
        {
            base.loadGlobalBotSettings();
        }
        
    }
    public class MasterUser : RTParser.UserImpl
    {
        public MasterUser(string UserID, Bot bot)
            : base(UserID, bot)
        {
        }
        public MasterUser(string UserID, AltBot bot)
            : base(UserID, bot)
        {
        }
    }
#if false
    sealed public class MasterRequest :
#if interface
        RequestImpl, 
#endif
 Request
    {
/*
        public Request(String rawInput, AltAIMLbot.User user, AltBot bot)
            : this(rawInput, user, bot, null)
        {
        }
      
              public MasterRequest(String rawInput, AltAIMLbot.User user, AltBot bot, AltAIMLParser.Request r)
            : base(rawInput, user, bot, r, null)
        {
        }
*/
        public MasterRequest(string rawInput, User user, string thatSaid, User targetUser, AltBot bot, Request parent, GraphMaster graphMaster)
            : base(rawInput, user, thatSaid, targetUser, bot, parent, graphMaster)
        {
        }
    }
#endif
    sealed public class MasterResult :
#if interface
 ResultImpl, 
#endif
        Result , InteractionResult 
    {
        public MasterResult(User user, AltBot bot, Request request)
            : base(user, bot, request)
        {
        }

        public MasterResult(string rawInput, User user, AltBot bot, Request parent, User targetUser)
            : base(rawInput, user, bot, parent, targetUser)
        {
        }

        #region InteractionResult Members


        public override Result result
        {
            get { return this;  }
        }

        public override RTParser.Variables.ISettingsDictionary RequesterChanges
        {
            get { throw new NotImplementedException(); }
        }

        public override RTParser.Variables.ISettingsDictionary ResponderChanges
        {
            get { throw new NotImplementedException(); }
        }

        public override InteractionResult PreviousInteraction
        {
            get { throw new NotImplementedException(); }
        }

        public override InteractionResult NextInteraction
        {
            get { throw new NotImplementedException(); }
        }

        public override Unifiable GetInputSentence(int sentence)
        {
            throw new NotImplementedException();
        }

        public override void FreeResult()
        {
            throw new NotImplementedException();
        }

        #endregion
    }
    
    namespace Utils
    {
        public class AIMLLoader : RTParser.Utils.AIMLLoaderU
        {
            public AIMLLoader(AltBot bot)
                : base(bot, bot == null ? null : bot.GetBotRequest("-AIMLLoader-"))
            {
            }
        }
    }

}
