using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using System.Xml;
using Cogbot;
using Cogbot.Actions;
using Cogbot.World;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;
using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;


using MushDLR223.ScriptEngines;

namespace SimExportModule
{
    public partial class ImportCommand 
    {
        private void MoveToKnownObjects()
        {
            lock (ORPHANS)
            {
                if (ORPHANS.Count > 0)
                {
                    foreach (PrimToCreate orphan in ORPHANS)
                    {
                        Exporting.AddMoveTo(orphan.SimPosition);
                    }
                    return;
                }

            }
            foreach (PrimToCreate parent in parents)
            {
                Exporting.AddMoveTo(parent.SimPosition);
            }
        }
    }
}
