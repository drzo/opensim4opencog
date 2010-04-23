using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class Say : Command, BotPersonalCommand
    {
        public Say(BotClient Client) 
            : base(Client) 
        {
            Name = GetType().Name.ToLower().Replace("command", "");
            Description = "Say a message for everyone to hear.";
            Usage = "To communicate to everyone, type \"say <message>\"";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);
           
            string text = args.str;

            if (args.str.Length > 0)
            {
                if (text.Contains("<sapi>"))
                {
                    // example fragment
                    // <sapi> <silence msec="100" /> <bookmark mark="anim:hello.csv"/> Hi there </sapi>
                    text = text.Replace("<sapi>", "");
                    text = text.Replace("</sapi>", "");
                    while (text.Contains("<"))
                    {
                        int p1 = text.IndexOf("<");
                        int p2 = text.IndexOf(">", p1);
                        if (p2 > p1)
                        {
                            string fragment = text.Substring(p1, (p2 + 1) - p1);
                            text = text.Replace(fragment, " ");
                        }
                    }

                }
                TheBotClient.Talk(text);
            }
            return Success("said: " + text);
        }
    }
}
