using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
//using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.WebUtil
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
            Description = "Do an http get. Usage: HttpGet http://localhost:5580/action?cmd=say&args=hello";
        }
        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            return Success(DoHttpGet(args.str));
        }
    }
}
