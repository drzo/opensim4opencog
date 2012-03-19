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
        private readonly List<Vector3> moveToPoints = new List<Vector3>();

        private void StopMoving()
        {
            shouldBeMoving = false;            
        }
                
        public void AddRegionWaypoints()
        {
            lock (moveToPoints) RecomputeGotos();
        }
        private void RecomputeGotos()
        {
            int lowZ = 23;// (int)Client.Self.SimPosition.Z;
            for (int z = lowZ; z < lowZ + 1000; z += 256)
            {
                for (int x = 16; x < 256; x += 16)
                {
                    for (int y = 16; y < 256; y += 16)
                    {
                        Vector3 moveto = new Vector3(x, y, z);
                        if (onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                            moveToPoints.Add(moveto);
                    }
                }
            }
            for (int z = lowZ + 1400; z < (lowZ + 4000); z += 300)
            {
                for (int x = 16; x < 256; x += 32)
                {
                    for (int y = 16; y < 256; y += 32)
                    {
                        Vector3 moveto = new Vector3(x, y, z);
                        if (onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                            moveToPoints.Add(moveto);
                    }
                }
            }
        }

        public void AddMoveTo(Vector3 v3)
        {
            var v30 = v3;
            v3.X = Math.Min(Math.Max(v3.X, 0), 256);
            v3.Y = Math.Min(Math.Max(v3.Y, 0), 256);
            v3.Z = Math.Min(Math.Max(v3.Z, 0), 5000);
            if (v3 != v30)
            {
                Success("Changed pos " + v30);
            }
            lock (moveToPoints) moveToPoints.Add(v3);
        }
        void MoveOnce()
        {
            while (true)
            {
                try
                {
                    MoveOnce0();
                   
                }
                catch
                {
                    Thread.Sleep(30000);
                }
            }
        }
        void MoveOnce0()
        {
            Client.Self.Movement.Fly = true;
            while (true)
            {
                if (!shouldBeMoving)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                var moveto = Vector3.Zero;
                if (moveToPoints.Count == 0)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                lock (moveToPoints)
                {
                    if (moveToPoints.Count == 1)
                    {
                        moveto = moveToPoints[0];
                        moveToPoints.Remove(moveto);
                        
                    } else
                    {
                        moveto = moveToPoints[rnd.Next(0, moveToPoints.Count - 1)];
                    }
                }
                if (moveto == Vector3.Zero 
                    || (Math.Abs(moveto.Z - Client.Self.SimPosition.Z) > 512 && moveToPoints.Count > 30) 
                    || moveto.Z > maxHeigth 
                    || !onlyObjectAt.IsInside(moveto.X, moveto.Y, moveto.Z))
                {
                    Thread.Sleep(100);
                    continue;
                }
                AttemptSitMover();
                var v3d = Client.Self.GlobalPosition;
                Vector3 pos = moveto;
                Vector3 at3d;

                //Client.Self   
                BeenTo(Client.Self.SimPosition);
                AttemptMoveTo(pos);
                at3d = Client.Self.SimPosition;
                Client.Self.Chat(string.Format("tptp,{0},{1},{2}", at3d.X, at3d.Y, at3d.Z), 4201, ChatType.Normal);
                BeenTo(Client.Self.SimPosition);
                Thread.Sleep(moveSleep * 1000);
                at3d = Client.Self.SimPosition;
                Client.Self.Chat(string.Format("tptp,{0},{1},{2}", at3d.X, at3d.Y, at3d.Z), 4201, ChatType.Normal);
                lock (moveToPoints)
                {
                    BeenTo(Client.Self.SimPosition);
                    if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 30)
                    {
                        moveToPoints.Remove(pos);
                    }
                }
            }
        }

        private void BeenTo(Vector3 at3d)
        {
            haveBeenTo.AddPoint(at3d);
            lock (moveToPoints) foreach (Vector3 point in LockInfo.CopyOf(moveToPoints))
            {
                if (Vector3.Distance(point, at3d) < 8)
                {
                    moveToPoints.Remove(point);
                }
            }
        }

        private void AttemptSitMover()
        {
            if (!TheSimAvatar.IsSitting)
            {
                var smo = WorldSystem.GetObject("cogbotscanchairRideForTwo");
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
            if (moverThread == null || !moverThread.IsAlive)
            {
                moverThread = new Thread(MoveOnce);
                moverThread.Name = "SimExport Mover";
                moverThread.Start();
            }
            shouldBeMoving = true;
        }

        public void AttemptMoveTo(Vector3 pos)
        {
            AttemptSitMover();
            if (pos.X < 1 || pos.Y < 1 || pos.Z < 10 || pos.X > 254 || pos.Y > 254 || pos.Z > 5000)
            {
                return;
            }
            SetCamera(pos);
            if (Vector3.Distance(Client.Self.SimPosition, pos) < 8)
            {
                BeenTo(Client.Self.SimPosition);
                return;
            }
            Success("Trying to get to " + pos);
            Client.Self.Chat(string.Format("tptp,{0},{1},{2}", pos.X, pos.Y, pos.Z), 4201, ChatType.Normal);
            if (!TheSimAvatar.IsSitting)
            {
                uint globalX, globalY;
                Utils.LongToUInts(RegionHandle, out globalX, out globalY);
                ExpectedGotoV3D =
                    new Vector3d(
                        (double) globalX + (double) pos.X,
                        (double) globalY + (double) pos.Y,
                        (double) pos.Z);
                var at3d = Client.Self.GlobalPosition;
                if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 32)
                {
                    SetCamera(pos);
                    return;
                }
                Client.Self.AutoPilotCancel();
                Client.Self.Teleport(RegionHandle, pos, pos);
                SetCamera(pos);
                Thread.Sleep(4000);
                at3d = Client.Self.GlobalPosition;
                if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 16)
                {
                    SetCamera(pos);
                    return;
                }
                Success("AutoPilot to get to " + pos);
                Client.Self.AutoPilot(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z);
                SetCamera(pos);
                Thread.Sleep(4000);
                Client.Self.AutoPilotCancel();
            }
        }

        private void SetCamera(Vector3 pos)
        {
            Client.Self.Movement.Fly = true;
            Client.Self.Movement.Camera.Position = pos;
            Client.Self.Movement.SendUpdate(true);
        }

        public void MoveCloseTo(SimObject exportPrim)
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

        public void MoveCloseTo(UUID exportPrim)
        {
            AttemptSitMover();
            Success("Trying to get to " + exportPrim);
            Client.Self.Chat(string.Format("" + exportPrim), 4201, ChatType.Normal);
        }
    }
}
