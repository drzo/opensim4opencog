using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    class TurnToCommand : cogbot.Actions.Command
    {
        public TurnToCommand(BotClient client)
        {
            Name = "turnto";
            Description = "turn the avatar toward the specified position for a maximum of seconds. turnto [prim | [x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            SimPosition simObject;

            if (args.Length > 3 || args.Length == 0)
                return "Usage: turnto [prim | [x y [z]]";

            Vector3 local = new Vector3();
            if (float.TryParse(args[0], out local.X) &&
                float.TryParse(args[1], out local.Y))
            {

                if (args.Length == 3)
                {
                    Single.TryParse(args[2], out local.Z);
                }
                else
                {
                    local.Z = GetSimPosition().Z;
                }
                Vector3d target = WorldSystem.TheSimAvatar.GetPathStore().LocalToGlobal(local);
                simObject = SimWaypointImpl.CreateGlobal(target);
            }
            else
            {
                string s = String.Join(" ", args);
                Primitive prim;


                if (WorldSystem.tryGetPrim(s, out prim))
                {

                    simObject = WorldSystem.GetSimObject(prim);
                    if (!simObject.IsRegionAttached())
                    {
                        return "Cannot get Sim Position of " + simObject;
                    }
                }
                else
                {
                    return "Cannot select " + s;
                }
            }

            WriteLine("ternto {0}", simObject);
            WorldSystem.TheSimAvatar.TurnToward(simObject);
            return WorldSystem.TheSimAvatar.DistanceVectorString(simObject);
        }
    }
}