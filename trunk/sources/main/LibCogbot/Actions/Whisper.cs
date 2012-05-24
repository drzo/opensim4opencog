using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    [Obsolete]
    class Whisper : Command, BotPersonalCommand, BotStatefullCommand
    {
        public UUID currentAvatar = UUID.Zero;
        public UUID currentSession = UUID.Zero;

        public Whisper(BotClient Client)
            : base(Client)
        {
            Description = "IM a user. Has nothing to do with SL 'whisper'";
            Details = AddUsage("whisper to <avatar name> <message>", "IM Avatar with Message") +
                    AddUsage("whisper <message>", "reply to the last person who IMed you");

            Parameters = CreateParams(
                "target", typeof (AgentSpec), "who you are IMing",
                "message", typeof (string), "what you IM");
            ResultMap = CreateParams(
                            "personFound", typeof(bool), "true iff we found the person to whisper to", 
                            "sentCorrect", typeof(bool), "true iff we successfully sent the message");
            currentAvatar = UUID.Zero;
            currentSession = UUID.Zero;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            string to = args["to"];

            if (to.Length > 0) {
                int argsUsed;
                List<SimObject> PS =
                    WorldSystem.GetPrimitives(to.Split(new[] {" "}, StringSplitOptions.RemoveEmptyEntries), out argsUsed);
                if (!IsEmpty(PS))
                {
                    foreach (var prim in PS)
                    {
                        currentAvatar = prim.ID;
                        break;
                    }
                }
                else
                {
                    SimAvatar avatar;
                    if (!WorldSystem.tryGetAvatar(to, out avatar))
                    {
                        return Failure("I don't know who " + to + "is.");
                    }
                    currentAvatar = avatar.ID;
                }

            }
            else if (currentAvatar == UUID.Zero)
            {
                return Failure("Please provide a name to whisper to.");
            }

            if (currentSession != UUID.Zero)
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase, currentSession);
            else
                Client.Self.InstantMessage(currentAvatar, args.objectPhrase);
            return Success("sent message");
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
           
        }

        #endregion
    }
}
