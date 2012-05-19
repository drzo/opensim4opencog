using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Crouch : Command, BotPersonalCommand
    {
       // bool isCrouching = false;

        public Crouch(BotClient Client)
            : base(Client)
        {
            Description = "Makes the bot crouch. Yup. Crouching. Important stuff";
            Usage = @"<p>crouch  - <i>crouch for 1/2 sec</i></p>
<p>crouch on  - <i>start crouching indefinitely</i></p>
<p>crouch off - <i>stop crouching</i></p>";
            Name = "Crouch";
            Category = CommandCategory.Movement;
            ParameterVersions = NamedParam.CreateParamVersions(
                NamedParam.CreateParams(),
                NamedParam.CreateParams(NamedParam.Optional("on", typeof(bool), "begin crouching")),
                NamedParam.CreateParams("off", typeof(bool), "stop crouching")
               );
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we crouched");
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            Client.describeNext = true;
            string[] tokens = args.tokens;
            //base.acceptInput(verb, args);
            if (tokens.Length == 0)
            {
                Client.Self.Crouch(true);
                Thread.Sleep(500);
               // isCrouching = false;
                Client.Self.Crouch(false);
                return Success("$bot crouched.");
            }
            else
                if (tokens[0].Equals("on"))
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = true;
                    return Success("$bot started crouching.");
                }
                else
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = false;
                    Client.Self.Crouch(false);
                    return Success("$bot done crouching.");
                }
        }

         
        
    }
}
