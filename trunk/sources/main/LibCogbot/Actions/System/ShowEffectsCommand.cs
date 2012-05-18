using System;
using cogbot;
using cogbot.Actions;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class ShowEffectsCommand : Command, RegionMasterCommand, BotStatefullCommand
    {
        bool ShowEffects = false;

        public ShowEffectsCommand(BotClient testClient)
            : base(testClient)
        {
            Name = "showeffects";
            Description = "Prints out information for every viewer effect that is received. Usage: showeffects [on/off]";
            Category = CommandCategory.Other;
        }

        void Avatars_ViewerEffectLookAt(object sender, ViewerEffectLookAtEventArgs e)
        {
            if (ShowEffects)
                WriteLine(
                "ViewerEffect [LookAt]: SourceID: {0} TargetID: {1} TargetPos: {2} Type: {3} Duration: {4} ID: {5}",
                e.SourceID.ToString(), e.TargetID.ToString(), e.TargetPosition, e.LookType, e.Duration,
                e.EffectID.ToString());
        }

        void Avatars_ViewerEffectPointAt(object sender, ViewerEffectPointAtEventArgs e)
        {
            if (ShowEffects)
                WriteLine(
                "ViewerEffect [PointAt]: SourceID: {0} TargetID: {1} TargetPos: {2} Type: {3} Duration: {4} ID: {5}",
                e.SourceID.ToString(), e.TargetID.ToString(), e.TargetPosition, e.PointType, e.Duration,
                e.EffectID.ToString());
        }

        void Avatars_ViewerEffect(object sender, ViewerEffectEventArgs e)
        {
            if (ShowEffects)
                WriteLine(
                "ViewerEffect [{0}]: SourceID: {1} TargetID: {2} TargetPos: {3} Duration: {4} ID: {5}",
                e.Type, e.SourceID.ToString(), e.TargetID.ToString(), e.TargetPosition, e.Duration,
                e.EffectID.ToString());
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                ShowEffects = true;
                return Success("Viewer effects will be shown on the console");
            }
            else if (args.Length == 1)
            {
                if (args[0] == "on")
                {
                    Client.Avatars.ViewerEffect += new EventHandler<ViewerEffectEventArgs>(Avatars_ViewerEffect);
                    Client.Avatars.ViewerEffectPointAt += new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
                    Client.Avatars.ViewerEffectLookAt += new EventHandler<ViewerEffectLookAtEventArgs>(Avatars_ViewerEffectLookAt);
                    ShowEffects = true;
                    return Success("Viewer effects will be shown on the console");
                }
                else
                {
                    ShowEffects = false;
                    return Success("Viewer effects will not be shown");
                }
            }
            else
            {
                return ShowUsage();
        }
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            Client.Avatars.ViewerEffect -= new EventHandler<ViewerEffectEventArgs>(Avatars_ViewerEffect);
            Client.Avatars.ViewerEffectPointAt -= new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
            Client.Avatars.ViewerEffectLookAt -= new EventHandler<ViewerEffectLookAtEventArgs>(Avatars_ViewerEffectLookAt);            
        }

        #endregion
    }
}
