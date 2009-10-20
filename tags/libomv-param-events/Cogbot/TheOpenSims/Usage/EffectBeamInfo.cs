using System;
using System.Collections.Generic;
using System.Timers;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    internal class EffectBeamInfo
    {
        readonly GridClient client;

        public EffectBeamInfo(GridClient client)
        {
            this.client = client;
            beamTimer = new System.Timers.Timer();
            beamTimer.Enabled = false;
            effectSource = client.Self.AgentID;
            beamTimer.Elapsed += new ElapsedEventHandler(beamTimer_Elapsed);
        }

        public Vector3d GlobalPosition(SimPosition prim)
        {
            return prim.GlobalPosition;
        }

        void beamTimer_Elapsed(object sender, EventArgs e)
        {
            if (beamID == null) return;

            try
            {
                client.Self.SphereEffect(GlobalPosition(targetPrim), beamColors[beamRandom.Next(0, 3)], 0.85f, sphereID);
                int i = 0;
                for (i = 0; i < numBeans; i++)
                {
                    UUID newBeam = UUID.Random();
                    Vector3d scatter;

                    if (i == 0)
                    {
                        scatter = GlobalPosition(targetPrim);
                    }
                    else
                    {
                        Vector3d direction = client.Self.GlobalPosition - GlobalPosition(targetPrim);
                        Vector3d cross = direction % new Vector3d(0, 0, 1);
                        cross.Normalize();
                        scatter = GlobalPosition(targetPrim) + cross * (i * 0.2d) * (i % 2 == 0 ? 1 : -1);
                    }
                    client.Self.BeamEffect(effectSource, UUID.Zero, scatter, beamColors[beamRandom.Next(0, 3)], 1.0f, beamID[i]);
                }

                for (int j = 1; j < numBeans; j++)
                {
                    UUID newBeam = UUID.Random();
                    Vector3d scatter;
                    Vector3d cross = new Vector3d(0, 0, 1);
                    cross.Normalize();
                    scatter = GlobalPosition(targetPrim) + cross * (j * 0.2d) * (j % 2 == 0 ? 1 : -1);

                    client.Self.BeamEffect(effectSource, UUID.Zero, scatter, beamColors[beamRandom.Next(0, 3)], 1.0f, beamID[j + i - 1]);
                }
            }
            catch (Exception) { };

        }

        private System.Timers.Timer beamTimer;
        private List<Vector3d> beamTarget;
        private Random beamRandom = new Random();
        private UUID pointID;
        private UUID sphereID;
        private List<UUID> beamID;
        private int numBeans;
        private Color4[] beamColors = new Color4[3] { new Color4(0, 255, 0, 255), new Color4(255, 0, 0, 255), new Color4(0, 0, 255, 255) };
        private SimPosition targetPrim;
        private UUID effectSource;

        public void SetPointing(SimPosition prim, int numBeans)
        {
            UnSetPointing();
            //client.Self.Movement.TurnToward(prim.Position);
            pointID = UUID.Random();
            sphereID = UUID.Random();
            beamID = new List<UUID>();
            beamTarget = new List<Vector3d>();
            targetPrim = prim;
            this.numBeans = numBeans;

            SimObject O = prim as SimObject;
            if (O != null)
            {
                client.Self.PointAtEffect(effectSource, O.ID, Vector3d.Zero, PointAtType.Select, pointID);
            }
            else
            {
                client.Self.PointAtEffect(effectSource, UUID.Zero, prim.GlobalPosition, PointAtType.Select, pointID);
            }

            for (int i = 0; i < numBeans; i++)
            {
                UUID newBeam = UUID.Random();
                beamID.Add(newBeam);
                beamTarget.Add(Vector3d.Zero);
            }

            for (int i = 1; i < numBeans; i++)
            {
                UUID newBeam = UUID.Random();
                beamID.Add(newBeam);
                beamTarget.Add(Vector3d.Zero);
            }

            beamTimer.Interval = 1000;
            beamTimer.Enabled = true;
        }

        public void UnSetPointing()
        {
            beamTimer.Enabled = false;
            if (pointID != UUID.Zero)
            {
                client.Self.PointAtEffect(effectSource, UUID.Zero, Vector3d.Zero, PointAtType.None, pointID);
                pointID = UUID.Zero;
            }

            if (beamID != null)
            {
                foreach (UUID id in beamID)
                {
                    client.Self.BeamEffect(UUID.Zero, UUID.Zero, Vector3d.Zero, new Color4(255, 255, 255, 255), 0, id);
                }
                beamID = null;
            }

            if (sphereID != UUID.Zero)
            {
                client.Self.SphereEffect(Vector3d.Zero, Color4.White, 0, sphereID);
                sphereID = UUID.Zero;
            }

        }
    }
}