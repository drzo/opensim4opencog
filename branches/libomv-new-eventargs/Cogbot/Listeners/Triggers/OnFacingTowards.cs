using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Listeners.Triggers
{
    public class OnFacingTowards
    {
        Primitive Mover;        
        BotClient Client;
        Primitive LastTarget = null;
        Vector3 Velosity = Vector3.Zero;
        List<Primitive> MoverFaceing = new List<Primitive>();
        int coneRadius = 10;
        int radius = 40;

        private Quaternion GetRotation(Vector3 src, Vector3 dest)
        {
            src.Normalize();
            dest.Normalize();

            float d = Vector3.Dot(src, dest);

            if (d >= 1f)
            {
                return Quaternion.Identity;
            }
            else if (d < (1e-6f - 1.0f))
            {
                Vector3 axis = Vector3.Cross(Vector3.UnitX, src);

                if (axis.LengthSquared() == 0)
                {
                    axis = Vector3.Cross(Vector3.UnitY, src);
                }

                axis.Normalize();
                return Quaternion.CreateFromAxisAngle(axis,(float) Math.PI);
            }
            else
            {
                float s = (float)Math.Sqrt((1 + d) * 2);
                float invS = 1 / s;

                Vector3 c = Vector3.Cross(src, dest);
                Vector3 invSc = c * new Vector3(invS, invS, invS);
                Quaternion q = new Quaternion(invSc, 0.5f * s);
                q.Normalize();

                return q;
            }
        }


        public OnFacingTowards(Primitive mover, BotClient bc)
        {
            Mover = mover;
            Client = bc;
        }

        void RegisterTrigger()
        {
            UpdateAll();
            Client.Objects.OnObjectUpdated += OnMovingTowards_OnObjectUpdated;
        }

        private void OnMovingTowards_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (update.LocalID == Mover.LocalID)
            {
                UpdateAll();
            }
            else
            {
                Primitive prim = Client.WorldSystem.GetPrimitive(update.LocalID);
                if (IsFacing(prim))
                {
                    if (!MoverFaceing.Contains(prim))
                        MoverFaceing.Add(prim);
                }
                else
                {
                    if (MoverFaceing.Contains(prim))
                        MoverFaceing.Remove(prim);
                }
            }
        }

        private void UpdateAll()
        {
            MoverFaceing = Client.Network.CurrentSim.ObjectsPrimitives.FindAll(
                delegate(Primitive prim)
                {
                    return IsFacing(prim);
                }
             );
        }

        //private void doObjectSensor(SenseRepeatClass ts, LSL_Types.list SensedObjects)
        //{
        //    List<EntityBase> Entities;


        //    Vector3 SensePoint = new Vector3(0, 0, 0);

        //    //Vector3 sensorPos = SensePoint.AbsolutePosition;
        //    //Vector3 regionPos = new Vector3(m_CmdManager.m_ScriptEngine.World.RegionInfo.RegionLocX * Constants.RegionSize, m_CmdManager.m_ScriptEngine.World.RegionInfo.RegionLocY * Constants.RegionSize, 0);
        //   // Vector3 fromRegionPos = sensorPos + regionPos;

        //    Quaternion q = SensePoint.RotationOffset;
        //    Quaternion r = new Quaternion(q.X, q.Y, q.Z, q.W);
        //    Vector3 forward_dir = (new Vector3(1, 0, 0) * r);
        //    double mag_fwd = Vector3.Mag(forward_dir);

        //    Vector3 ZeroVector = new Vector3(0, 0, 0);


        //    foreach (EntityBase ent in Entities)
        //    {

        //        Vector3 toRegionPos = ent.AbsolutePosition + regionPos;
        //        double dis = Math.Abs((double)Util.GetDistanceTo(toRegionPos, fromRegionPos));
        //        if (keep && dis <= ts.range && ts.host.UUID != ent.UUID)
        //        {
        //            // In Range and not the object containing the script, is it the right Type ?
        //            int objtype = 0;

        //            SceneObjectPart part = ((SceneObjectGroup)ent).RootPart;

        //            // If any of the objects attributes match any in the requested scan type
        //            if (((ts.type & objtype) != 0))
        //            {
        //                // Right type too, what about the other params , key and name ?
        //                if (ts.arc < Math.PI)
        //                {
        //                    // not omni-directional. Can you see it ?
        //                    // vec forward_dir = llRot2Fwd(llGetRot())
        //                    // vec obj_dir = toRegionPos-fromRegionPos
        //                    // dot=dot(forward_dir,obj_dir)
        //                    // mag_fwd = mag(forward_dir)
        //                    // mag_obj = mag(obj_dir)
        //                    // ang = acos(dot /(mag_fwd*mag_obj))
        //                    double ang_obj = 0;
        //                    try
        //                    {
        //                        Vector3 diff = toRegionPos - fromRegionPos;
        //                        Vector3 obj_dir = new Vector3(diff.X, diff.Y, diff.Z);
        //                        double dot = Vector3.Dot(forward_dir, obj_dir);
        //                        double mag_obj = Vector3.Mag(obj_dir);
        //                        ang_obj = Math.Acos(dot / (mag_fwd * mag_obj));
        //                    }
        //                    catch
        //                    {
        //                    }

        //                    if (ang_obj > ts.arc) keep = false;
        //                }

        //                if (keep == true)
        //                {
        //                    // add distance for sorting purposes later
        //                    SensedObjects.Add(dis);
        //                    SensedObjects.Add(ent.UUID);
        //                }
        //            }
        //        }
        //    }
        //}


      /*
         
            point01 = [15, 15, 15] --#your first point
            point02 = [10,19,-9] --#your second point
            theVector = point02-point01 --#this is the vector defined by them, looking from 1 at 2
            p1 = point pos:point01 --#just for visualization, let's create Point Helpers to see the points
            p2 = point pos:point02

            theZ = normalize theVector --#this is the looking axis (Z)
            theUp = [0,0,1] --#take the world Z as the up vector
            theX = normalize (cross theUp theZ) --#calculate an X axis orthogonal to both
            theY = normalize (cross theZ theX) --#calculate a Y axis orthogonal to X and Z
            theTM = matrix3 theX theY theZ point01 --#build a matrix from the 3 vectors 
            theQuat = (inverse theTM) as quat --#convert the inverse of the matrix to a quaternion value
            theEuler = theQuat as EulerAngles --#then convert the Quat to Euler. 

            --#Again, for visulazation purposes only, let's take some object like a cone 
            c = Cone height:(length theVector) radius1:5 radius2:0
            c.rotation = theEuler --#set the rotation of the cone to the Euler angles we calculated
            c.pos = point01 --#and place the cone at the first point 
        */
        public bool IsFacing(Vector3 relative, Quaternion rotation) {
            Vector3 theZ = Vector3.Normalize(relative);
            Vector3 theUP = new Vector3(0,0,1);
            Vector3 theX = Vector3.Normalize(Vector3.Cross(theUP,theZ));
            Vector3 theY = Vector3.Normalize(Vector3.Cross(theZ,theX));
            Quaternion rot = GetRotation(Vector3.Zero, relative);
           
            Quaternion qdiff = rotation - rot;


          //  Quaternion q = Vector3.RotationBetween(
        //    float zrotC = Math.Sin(relative.Y);
            return false;
            
        }
        public bool IsFacing(Primitive prim)
        {
            return IsFacing(Mover.Position - prim.Position, Mover.Rotation);
        }
    }
}
