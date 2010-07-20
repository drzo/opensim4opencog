using System.Windows.Forms;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Pathfinder
{
    public class WalkToCommand : cogbot.Actions.Command, BotPersonalCommand
    {
        public WalkToCommand(BotClient client)
        {
            Name = "WalkTo";
            Description = "Go to the avatar toward the specified position for a maximum of seconds. Usage: WalkTo [prim | [x y]] [dist]";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            float distance = 2.0f;

            int argsUsed;
            SimPosition simObject = WorldSystem.GetVector(args, out argsUsed);

            if (simObject==null) return Failure("Cannot find " + string.Join(" ", args)); 
            if (!simObject.IsRegionAttached)
            {
                return Failure("Cannot get SimPosition of " + simObject);
            }

            distance = 0.5f + simObject.GetSizeDistance();
            if (argsUsed < args.Length)
            {
                float d;
                if (float.TryParse(args[argsUsed], out d))
                {
                    distance = d;
                }
            }
            WriteLine("WalkTo {0} {1}", simObject, distance);
            WorldSystem.TheSimAvatar.MoveTo(simObject.GlobalPosition, distance, 10);
            WorldSystem.TheSimAvatar.StopMoving();
            return Success(WorldSystem.TheSimAvatar.DistanceVectorString(simObject));
        }

        private void Goto(Vector3 target, float p)
        {

            if (true)
            {
                uint x, y;
                Utils.LongToUInts(Client.Network.CurrentSim.Handle, out x, out y);
                Vector2 v2 = new Vector2(target.X, target.Y);
                Vector2 cp = new Vector2(GetSimPosition().X, GetSimPosition().Y);
                float d = Vector2.Distance(v2, cp);
                float dl = d;
                bool autoOff = false;
                while (d > p)
                {
                    if (autoOff)
                    {
                        Client.Self.Movement.TurnToward(target);
                        Client.Self.AutoPilot((ulong)(x + target.X), (ulong)(y + target.Y), GetSimPosition().Z);
                        autoOff = false;
                    }
                    cp = new Vector2(GetSimPosition().X, GetSimPosition().Y);
                    d = Vector2.Distance(v2, cp);
                    if (dl < d)
                    {
                        Client.Self.AutoPilotCancel();
                        autoOff = true;
                        Client.Self.Movement.TurnToward(target);
                        Client.Self.Movement.Stop = true;
                        Client.Self.Movement.AtPos = false;
                        Client.Self.Movement.NudgeAtPos = true;
                        Client.Self.Movement.SendUpdate(true);
                        Client.Self.Movement.NudgeAtPos = false;
                        Client.Self.Movement.SendUpdate(true);
                    }
                    //Thread.Sleep(10);
                    Application.DoEvents();
                    dl = d;
                }
                Client.Self.Movement.TurnToward(target);
                Client.Self.AutoPilotCancel();
                return;
            }
        }
    }
}