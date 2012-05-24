using System;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class MD5Command : Command, SystemApplicationCommand
    {
        public MD5Command(BotClient testClient)
        {
            Name = "md5";
            Description = 
@"Creates an MD5 hash from a given string. The string must be double quoted if it contains blanks.
Currently the function isn't that useful, since the underlying C# md5 pads short strings to 32 bytes
producing a different hash than LSL produces";
            Details = Example(
                @"md5 ""tacos of god:12873""
prints
[19:53] md5: Success: $1$08b90c2ebcce5d7f46176eb7c05af0ea
the LSL script
default
{
    state_entry()
    {
        llSay(0, llMD5String(""tacos of god"", 12873));
    }
}

prints
[19:50]  Primitive: 6e130eabfe1f809eb6399796803e0d81
", "notice the colon in the md5 command, and that the results don't match");
            Parameters = CreateParams(
                "string", typeof(string), "string to compute md5 hash of");

            Category = CommandCategory.Security;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 1)
                return Success(Utils.MD5(args[0]));
            else
                return ShowUsage();// " md5 [password]";
        }
    }
}
