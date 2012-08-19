using System;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    /// <summary>
    /// Set avatars current appearance to appearance last stored on simulator
    /// </summary>
    public class AppearanceCommand : Command, BotPersonalCommand, FFIComplete
    {
		public AppearanceCommand(BotClient testClient)
        {
            Name = "appearance";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description = @"Set your current appearance to your last saved appearance.  Makes sure the bot is not a cloud.
<p>Adding 'nobake' doesn't rebake the avatar's textures.</p>
<p>Adding 'wait' blocks until server informs the appreance is set.</p>";
		    Details = AddUsage("appearance [nobake][wait][send]", "tells the server to use the last cached appearence baking or not") +
                    AddExample("appearance", "Same as rebaking in a normal client") +
                    AddExample("appearance wait", "Same as rebaking in a normal client") +
		            AddExample("appearance nobake", "fast way to tell the server you are not a cloud");
		    Parameters =
		        CreateParams(Optional("nobake", typeof (bool), "Do not rebake the avatar's textures"),
                                        Optional("wait", typeof(bool), "Wait until server informs the appreance is set"),
                                        Optional("send", typeof(bool), "Let server decide if no baking is needed or not"));
		    ResultMap = CreateParams(
		        "message", typeof (string), "if success was false, the reason why",
		        "success", typeof (bool), "true if outfit was worn");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            AutoResetEvent appearanceEvent = new AutoResetEvent(false);
            EventHandler<AppearanceSetEventArgs> callback = (s, e) => appearanceEvent.Set();

            // Start the appearance setting process (with baking enabled or disabled)
            bool bake = !args.IsTrue("nobake");
            bool send = args.IsTrue("send");
            bool wait = args.IsTrue("wait");
            if (wait)
            {
                // Register a handler for the appearance event
                Client.Appearance.AppearanceSet += callback;
                
            }
            if (send)
            {
                Client.Appearance.RequestSetAppearance();
            }
            else
            {
                Client.Appearance.RequestSetAppearance(bake);
            }
            if (wait)
            {
                bool success = false;
                //// Wait for the process to complete or time out
                if (appearanceEvent.WaitOne(1000 * 120, false))
                    success = true;

                //// Unregister the handler
                Client.Appearance.AppearanceSet -= callback;

                //// Return success or failure message
                if (!success) return Failure("Timed out while setting appearance");
            }
            return Success("Successfully set appearance bake=" + bake);
        }
    }
}
