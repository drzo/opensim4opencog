using System;
using Cogbot;
using Cogbot.Actions;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class ShowEffectsCommand : Command, RegionMasterCommand, BotStatefullCommand
    {
        bool ShowEffects = false;

        public ShowEffectsCommand(BotClient testClient)
            : base(testClient)
        {
            Name = "showeffects";
            Description = "Prints out information for every viewer effect that is received. Usage: showeffects [on/off]";
            Details = AddUsage("showeffects on", "Turn on effect listing") +
                AddUsage("showeffects off", "Turn off effect listing") +
                Example(@"/showeffects on
[14:53] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: a910f6af-de58-4106-8b90-37f1fca9695f TargetPos: <-0.840918242931366, -0.136444821953774, -0.523678779602051> Type: FreeLook Duration: 2 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:53] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: 5d9b2843-cd64-480c-9524-74e607629bae TargetPos: <0, 0, 0> Type: Focus Duration: 1.701412E+38 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:53] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: a910f6af-de58-4106-8b90-37f1fca9695f TargetPos: <5, 0, 0> Type: Focus Duration: 1.701412E+38 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:54] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: 5d9b2843-cd64-480c-9524-74e607629bae TargetPos: <0, 0, 0> Type: Focus Duration: 1.701412E+38 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:54] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: 37cbc2e3-c799-43c4-a6e5-2a5464dacd35 TargetPos: <-6.58285188674927, -0.108557350933552, -5.84904813766479> Type: Focus Duration: 1.701412E+38 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:54] ViewerEffect [Beam]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: 00000000-0000-0000-0000-000000000000 TargetPos: <256140.954620361, 256326.824554443, -6.3584098815918> Duration: 1 ID: d1f9baeb-b88c-d5c8-30b8-67b1beec443c
[14:54] ViewerEffect [LookAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: b8f9f438-4c99-4e30-8dad-c1a2580b72c1 TargetPos: <0, 0, 0> Type: Select Duration: 1.701412E+38 ID: f263233c-4f93-09e3-cae3-9c8f1675d282
[14:54] ViewerEffect [PointAt]: SourceID: a910f6af-de58-4106-8b90-37f1fca9695f TargetID: b8f9f438-4c99-4e30-8dad-c1a2580b72c1 TargetPos: <0, 0, 0> Type: Select Duration: 8.507059E+37 ID: 0aad7923-7a8b-712b-918e-5d17d06cd42b
[14:54] ViewerEffect [LookAt]: SourceID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetPos: <-0.860213756561279, -0.469168335199356, -0.19978304207325> Type: FreeLook Duration: 2 ID: 65afbdef-6a7c-ed45-bfdc-d322b9690caf
[14:54] ViewerEffect [LookAt]: SourceID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetPos: <1.99089336395264, -0.19064100086689, 0> Type: Idle Duration: 3 ID: 65afbdef-6a7c-ed45-bfdc-d322b9690caf
[14:54] ViewerEffect [LookAt]: SourceID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetPos: <-0.860213756561279, -0.469168335199356, -0.19978304207325> Type: FreeLook Duration: 2 ID: 65afbdef-6a7c-ed45-bfdc-d322b9690caf
[14:54] Primitive: Script running
[14:54] ViewerEffect [LookAt]: SourceID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetID: 8f6ce54e-95f5-46d0-b090-c5361c821232 TargetPos: <1.99089336395264, -0.19064100086689, 0> Type: Idle Duration: 3 ID: 65afbdef-6a7c-ed45-bfdc-d322b9690caf
/showeffects off", "turn effects on, see some, turn off");
            ParameterVersions = CreateParamVersions(
                CreateParams("on", typeof(bool), "turn on listing"),
                CreateParams("off", typeof(bool), "turn off listing"));
            ResultMap = CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if we showed effects");
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

        public override CmdResult ExecuteRequest(CmdRequest args)
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
