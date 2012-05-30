using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Unmute : Mute
    {
        public Unmute(BotClient Client)
            : base(Client)
        {
            Description = "Remove avatars or objects from the mute list";
            Details = "<p>unmute all</p><p>unmute Fluffybunny Resident</p>";
        }
    }

    class Mute : Command, BotPersonalCommand
    {
        public Mute(BotClient Client)
            : base(Client)
        {
            Description = "Mute avatars or objects, or display the mute list. unmute unmutes.";
            Details = "<p>mute Fluffybunny Resident</p>" +
"<p>mute Particles &lt;primspec&gt;  mutes all but a specific element from:</p>" +
@"<ul>
<li>Default - mute everything</li>
<li>TextChat - don't mute text chat</li>
<li>VoiceChat - don't mute voice</li>
<li>Particles - don't mute particles</li>
<li>ObjectSounds - don't mute sounds</li>
<li>All - don't mute anything</li>
</ul>";
            ParameterVersions = CreateParamVersions(
                CreateParams(
                   Optional("element", typeof(MuteFlags), "element to not mute"),
                   "av", typeof(AgentSpec),
                   "Avatar to mute"),
                CreateParams(
                   Optional("element", typeof(MuteFlags), "element to not mute"),
                   "object", typeof(PrimSpec),
                   "object to mute"));
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we muted the object");
         }

        public override CmdResult acceptInput(string verb, Parser pargs, OutputDelegate WriteLine)
        {
            string[] args = pargs.tokens;
            var chat = TheBotClient.Self;
            string arg1 = "show";
            if (args.Length > 0)
            {
                arg1 = args[0].ToLower();
            }

            var cmld = chat.MuteList.Copy();

            if (arg1 == "show" || arg1 == "list" || arg1 == "request")
            {
                if (arg1 == "request") chat.RequestMuteList();
                int nfound = 0;
                foreach (var mm in cmld)
                {
                    Success("Mutelist Item: " + mm.Key + " is " + Helpers.StructToString(mm.Value));
                    nfound++;
                }
                return Success(verb + " found: " + nfound + " object/agent(s)");
            }

            lock (cmld)
            {
                cmld.Clear();
            }
            bool unmute = verb.ToLower().StartsWith("u");

            if (arg1 == "all")
            {
                if (unmute)
                {
                    int nfound = 0;

                    foreach (var mm in cmld)
                    {
                        MuteEntry me = mm.Value;
                        chat.UpdateMuteListEntry(me.Type, me.ID, me.Name, MuteFlags.All);
                        chat.RemoveMuteListEntry(me.ID, me.Name);
                        Success("Unmuted " + Helpers.StructToString(me) + ".");
                        nfound++;
                    }
                    chat.RequestMuteList();
                    return Success(verb + " found: " + nfound + " object/agent(s)");
                }
                if (args.Length == 1) args = new[] {"dist", "30"};
            }

            object value;
            MuteFlags mf = MuteFlags.All;
            int argsUsed;
            if (TryEnumParse(typeof(MuteFlags), args, 0, out argsUsed, out value))
            {
                mf = (MuteFlags)value;
            }
            if (!unmute)
            {
                // invert the flags 
                mf = (MuteFlags)((int)MuteFlags.All - (int)mf);
            }
            // if flags found
            if (argsUsed > 0)
            {
                args = Parser.SplitOff(args, argsUsed);
            }
            string muteName = ((MuteFlags) ((int) MuteFlags.All - (int) mf)).ToString();
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (!IsEmpty(PS))
            {
                int nfound = 0;
                foreach (var prim in PS)
                {
                    UUID id = prim.ID;
                    MuteType mt = MuteType.Object;
                    if (prim is SimAvatar)
                    {
                        mt = MuteType.Resident;
                    }
                    if (mf == MuteFlags.All) chat.RemoveMuteListEntry(id, null); else chat.UpdateMuteListEntry(mt, id, null, mf);
                    
                    nfound++;
                    Success(verb + " " + muteName + " " + prim + " " + id + ".");
                }                
                if (nfound > 0)
                {
                    chat.RequestMuteList();
                    return Success(verb + " " + muteName + ": " + nfound + " object/agent(s)");
                }
            }
            return Failure("I don't know who " + pargs.str + " is.");
        }
    }
}
