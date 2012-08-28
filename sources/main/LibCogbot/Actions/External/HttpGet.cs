using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
//using OpenMetaverse; //using libsecondlife;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.WebUtil
{
    public class HttpGet : Command, SystemApplicationCommand
    {
        public static string DoHttpGet(string url)
        {
            return Encoding.UTF8.GetString((new WebClient()).DownloadData(url));
        }

        public HttpGet(BotClient Client)
            : base(Client)
        {
            Name = "HttpGet";
            TheBotClient = Client;
        }

        public override void MakeInfo()
        {
            Description = "Do an http get.";
            Details = AddUsage(Name + " url", "read the contents of a URL to return result") +
                      AddExample(Name + " http://localhost:5580/action?cmd=say&args=hello",
                                 "makes the bot say something");
            Parameters = CreateParams("url", typeof (Uri), "url to get");
            Category = CommandCategory.Simulator;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            return Success(DoHttpGet(args.str));
        }
    }
}