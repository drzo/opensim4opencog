using System;
using System.Collections.Generic;
using System.Text;
using RTParser;

namespace AIMLbot
{
    public class Bot : RTParser.RTPBot
    {
    }
    public class User : RTParser.User
    {
        public User(string UserID, RTPBot bot)
            : base(UserID, bot)
        {
        }
    }
    public class Request : RTParser.Request
    {
        public Request(String rawInput, RTParser.User user, RTPBot bot)
            : base(rawInput, user, bot)
        {
        }
    }

    public class Result : RTParser.Result
    {
        public Result(RTParser.User user, RTPBot bot, RTParser.Request request)
            : base(user, bot, request)
        {

        }
    }

    namespace Utils
    {
        public class AIMLLoader : RTParser.Utils.AIMLLoader
        {
            public AIMLLoader(RTPBot bot)
                : base(bot,bot.BotAsUser)
            {
            }
        }
    }

}
