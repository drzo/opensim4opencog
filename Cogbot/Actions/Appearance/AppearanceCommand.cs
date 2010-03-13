using System;
using System.Threading;
using OpenMetaverse;

namespace cogbot.Actions.Appearance
{
    /// <summary>
    /// Set avatars current appearance to appearance last stored on simulator
    /// </summary>
    public class AppearanceCommand : Command, BotPersonalCommand
    {
		public AppearanceCommand(BotClient testClient)
        {
            Name = "appearance";
            Description = "Set your current appearance to your last saved appearance";
            Category = CommandCategory.Appearance;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            bool success = false;

            // Register a handler for the appearance event
            //AutoResetEvent appearanceEvent = new AutoResetEvent(false);
            //AppearanceManager.AppearanceUpdatedCallback callback =
            //    delegate(Primitive.TextureEntry te) { appearanceEvent.Set(); };
           // Client.Appearance.OnAppearanceUpdated += callback;

            // Start the appearance setting process (with baking enabled or disabled)
            bool bake = !(args.Length > 0 && args[0].Equals("nobake"));
            bool send = (args.Length > 0 && args[0].Equals("send"));
            if (send)
            {
                Client.Appearance.RequestSetAppearance();
                return Success( "Sent Appearance");
            }
            Client.Appearance.RequestSetAppearance(bake);
            return Success("Requested SetAppearance bake = " + bake);
            //// Wait for the process to complete or time out
            //if (appearanceEvent.WaitOne(1000 * 120, false))
            //    success = true;

            //// Unregister the handler
            //Client.Appearance.OnAppearanceUpdated -= callback;

            //// Return success or failure message
            //if (success)
            //    return Success("Successfully set appearance";
            //else
            //    return Success("Timed out while setting appearance";
        }
    }
}
