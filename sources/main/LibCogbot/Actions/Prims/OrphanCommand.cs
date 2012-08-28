using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.SimExport
{
    public class OrphanCommand : Cogbot.Actions.Command, RegionMasterCommand, AsynchronousCommand
    {
        public OrphanCommand(BotClient client)
        {
            Name = "orphans";
        }

        public override void MakeInfo()
        {
            Description = "Finds objects without locations [prim]";
            Category = Cogbot.Actions.CommandCategory.Objects;
            Parameters = CreateParams("targets", typeof (PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            ICollection<SimObject> objs;
            if (!args.TryGetValue("targets", out objs))
            {
                int argsUsed;
                objs = WorldSystem.ResolveCollection("$regionprims", out argsUsed);
            }
            int detatched = 0;
            int orphans = 0;
            int missingSculpties = 0;
            foreach (SimObjectImpl o in objs)
            {
                if (!o.IsRoot)
                {
                    if (o.Parent == null)
                    {
                        orphans++;
                        WriteLine("Orphans: " + o);
                    }
                }
                if (!o.IsRegionAttached)
                {
                    detatched++;
                    WriteLine("Detatched " + o);
                }

                if (o.IsSculpted)
                {
                    Primitive p = o.Prim;
                    if (WorldSystem.StartTextureDownload(p.Sculpt.SculptTexture) == null)
                    {
                        missingSculpties++;
                        WriteLine("IsSculpted " + o);
                    }
                }
            }
            return
                Success("object examinined " + objs.Count + " detacted: " + detatched + " orphans: " + orphans +
                        " missingScuplty: " + missingSculpties);
        }
    }
}