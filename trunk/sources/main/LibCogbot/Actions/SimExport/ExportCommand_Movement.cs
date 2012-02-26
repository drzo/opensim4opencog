using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;
using PathSystem3D.Mesher;

namespace cogbot.Actions.SimExport
{

    public partial class ExportCommand : Command, RegionMasterCommand
    {
        private bool shouldBeMoving = false;
        private Random rnd = new Random(DateTime.Now.Millisecond);
        private int moveSleep = 60;
        Box3Fill haveBeenTo = new Box3Fill(true);
        private Vector3d ExpectedGotoV3D;
        private Thread moverThread;
        private float maxHeigth = 4000f;
        private Vector3[] moveToPoints = new Vector3[6300];
        private int MaxGotos = 0;

        private void StopMoving()
        {
            shouldBeMoving = false;
        }
                
        private int RecomputeGotos()
        {
            MaxGotos = 0;
            int lowZ = (int)Client.Self.SimPosition.Z;
            for (int z = lowZ; z < lowZ + 1000; z += 256)
            {
                for (int x = 16; x < 256; x += 32)
                {
                    for (int y = 16; y < 256; y += 32)
                    {
                        Vector3 moveto = new Vector3(x, y, z);
                        if (onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                            moveToPoints[MaxGotos++] = moveto;
                    }
                }
            }
            for (int z = lowZ + 1400; z < (lowZ + 4000); z += 300)
            {
                for (int x = 16; x < 256; x += 64)
                {
                    for (int y = 16; y < 256; y += 64)
                    {
                        Vector3 moveto = new Vector3(x, y, z);
                        if (onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                            moveToPoints[MaxGotos++] = moveto;
                    }
                }
            }
            return MaxGotos;
        }

        void MoveOnce()
        {
            Client.Self.Movement.Fly = true;
            MaxGotos = RecomputeGotos();
            while (true)
            {
                int currentLoc = 0;
                if (!shouldBeMoving)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                AttemptSitMover();
                currentLoc = rnd.Next(0, MaxGotos - 1);
                var moveto = moveToPoints[currentLoc];
                if (moveto == Vector3.Zero || Math.Abs(moveto.Z - Client.Self.SimPosition.Z) > 512 || moveto.Z > maxHeigth || !onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                {
                    Thread.Sleep(100);
                    continue;
                }
                var v3d = Client.Self.GlobalPosition;
                Vector3 pos = moveToPoints[currentLoc];

                //Client.Self   
                AttemptMoveTo(pos);
                Thread.Sleep(moveSleep * 1000);
                haveBeenTo.AddPoint(Client.Self.SimPosition);
                var at3d = Client.Self.GlobalPosition;
                if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 30)
                {
                    moveToPoints[currentLoc] = Vector3.Zero;
                }
            }
        }

        private void AttemptSitMover()
        {
            if (!TheSimAvatar.IsSitting)
            {
                var smo = WorldSystem.GetObject("SitMover4200");
                if (smo == null) return;
                var sm = smo.ID;
                if (!CogbotHelpers.IsNullOrZero(sm))
                {
                    //SimObject SitMover = WorldObjects.GetSimObjectFromUUID(sm);
                    //if (SitMover != null)
                    {
                        Client.Self.RequestSit(sm, Vector3.Zero);
                    }
                }
            }
        }

        private void BeginMoving()
        {
            RecomputeGotos();
            if (moverThread == null || !moverThread.IsAlive)
            {
                moverThread = new Thread(MoveOnce);
                moverThread.Name = "SimExport Mover";
                moverThread.Start();
            }
            shouldBeMoving = true;
        }

        private void AttemptMoveTo(Vector3 pos)
        {
            AttemptSitMover();
            if (pos.X < 1 || pos.Y < 1 || pos.Z < 10)
            {
                return;
            }
            Success("Trying to get to " + pos);
            Client.Self.Movement.Camera.Position = pos;
            Client.Self.Movement.SendUpdate(true);
            if (TheSimAvatar.IsSitting)
            {
                Client.Self.Chat(string.Format("tptp,{0},{1},{2}", pos.X, pos.Y, pos.Z), 4201, ChatType.Normal);
            }
            else
            {
                uint globalX, globalY;
                Utils.LongToUInts(RegionHandle, out globalX, out globalY);
                ExpectedGotoV3D =
                    new Vector3d(
                        (double) globalX + (double) pos.X,
                        (double) globalY + (double) pos.Y,
                        (double) pos.Z);
                Client.Self.AutoPilotCancel();
                Client.Self.Teleport(RegionHandle, pos, pos);
                Thread.Sleep(4000);
                var at3d = Client.Self.GlobalPosition;
                if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 16)
                {
                   return;
                }
                Success("AutoPilot to get to " + pos);
                Client.Self.AutoPilot(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z);
                Thread.Sleep(4000);
                Client.Self.AutoPilotCancel();
            }
        }

        private void MoveCloseTo(SimObject exportPrim)
        {
            Vector3 up = exportPrim.UsePosition.SimPosition;
            if (up.X < 1)
            {
                return;
            }
            var dist = Vector3.Distance(Client.Self.SimPosition, up);
            if (dist > 10)
            {
                AttemptMoveTo(up);
            }
        }
    }
}
