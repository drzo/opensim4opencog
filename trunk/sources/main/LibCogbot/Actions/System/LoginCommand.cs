using System;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using Radegast;

namespace Cogbot.Actions.System
{
    internal class Login : Command, BotSystemCommand, SynchronousCommand
    {
        public Login(BotClient Client)
            : base(Client)
        {
            Name = "Login";
        }

        public override void MakeInfo()
        {
            Description = "Log into grid";
            Parameters = CreateParams(
                "first", typeof (string), "first name of bot",
                "last", typeof (string), "last name of bot",
                "pass", typeof (string), "password for bot",
                Optional("loginuri", typeof (Uri), "login uri for grid"),
                Optional("location", typeof (string), "one of home,last, or a sim name"));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            BotClient Client = TheBotClient;
            if (Client.IsLoggedInAndReady) return Success("Already logged in");
            {
                string value;
                if (args.TryGetValue("first", out value))
                {
                    Client.BotLoginParams.FirstName = value;
                }
                if (args.TryGetValue("last", out value))
                {
                    Client.BotLoginParams.LastName = value;
                }
                if (args.TryGetValue("pass", out value))
                {
                    Client.BotLoginParams.Password = value;
                }
                if (args.TryGetValue("loginuri", out value))
                {
                    Radegast.GridManager gm = new GridManager();
                    gm.LoadGrids();
                    string url = value;
                    string find = url.ToLower();
                    foreach (var grid in gm.Grids)
                    {
                        if (find == grid.Name.ToLower() || find == grid.ID.ToLower())
                        {
                            url = grid.LoginURI;
                        }
                    }
                    Client.BotLoginParams.URI = url;
                }
                if (args.TryGetValue("location", out value))
                {
                    Client.BotLoginParams.Start = value;
                }
                if (!Client.Network.Connected && !Client.Network.LoginMessage.StartsWith("Logging"))
                {
                    Client.Settings.LOGIN_SERVER = TheBotClient.BotLoginParams.URI;
                        // ClientManager.SingleInstance.config.simURL; // "http://127.0.0.1:8002/";
                    ///                    Client.Network.Login(Client.BotLoginParams.FirstName, Client.BotLoginParams.LastName, Client.BotLoginParams.Password, "OnRez", "UNR");
                    WriteLine("$bot beginning login");
                    Client.Login();
                    WriteLine("$bot started login");
                }
                else
                    return Success("$bot is already logged in.");
            }
            return Success("loging in...");
        }
    }
}