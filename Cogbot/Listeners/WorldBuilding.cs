using System;
using System.Collections.Generic;
using System.Threading;
using DotLisp;
using LitJson;
using OpenMetaverse.StructuredData;
using cogbot.TheOpenSims;
using OpenMetaverse;
using System.Reflection;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{

    public partial class WorldObjects
    {

        public static List<NamedParam> GetMemberValues(Object properties)
        {
            List<NamedParam> dict = new List<NamedParam>();
            if (properties == null)
            {
                return dict;
            }
            Type t = properties.GetType();
            foreach (FieldInfo o in t.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public))
            {
                try
                {
                    var v = o.GetValue(properties);
                    if (v == null) v = new NullType(o.FieldType);
                    dict.Add(new NamedParam(o.Name, o.FieldType, v));
                }
                catch (Exception e)
                {
                    Console.WriteLine("" + e);
                }
            }
            foreach (PropertyInfo o in t.GetProperties(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public))
            {
              if (o.CanRead)
              {
                  try
                  {
                      var v = o.GetValue(properties, null);
                      if (v == null) v = new NullType(o.PropertyType);
                      dict.Add(new NamedParam(o.Name, o.PropertyType, v));
                  }
                  catch (Exception e)
                  {
                      Console.WriteLine("" + e);
                  }
              }
            }
            return dict;
        }

        static public NamedParam ToParameter(string p, object s)
        {
            while (s is NamedParam)
            {
                NamedParam nv = (NamedParam)s;
                s = nv.Value;
            }
            return new NamedParam(p, s);
        }
        static public NamedParam ToParameter(string p, string type, object s)
        {
            return new NamedParam(new NamedParam(p, type), s);
        }

        static public NamedParam AsEffectID(UUID id)
        {
            return new NamedParam("id", id);
        }

        public void RescanTypes()
        {
            int count = SimObjects.Count;
            WriteLine("Rescaning " + count + " simobjects");
            foreach (SimObject obj in GetAllSimObjects())
            {
                //obj._Parent = obj.Parent;
                obj.Properties = null;
                obj.UpdateProperties(obj.Properties);
            }
            if (count != SimObjects.Count)
            {
                RescanTypes();
            }
        }

        public object ObjectToLisp(object prim)
        {
            OSD data = OSD.FromObject(prim);
            if (data.Type!=OSDType.Unknown)
                return SerializeLisp(data);
            Type t = prim.GetType();
            foreach (FieldInfo PI in t.GetFields(BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public))
            {
                object value = ToLispValue( PI.FieldType, PI.GetValue(prim));
                object kv = new Cons(PI.Name, new Cons(value, null));

            }
            return prim;
        }

        public bool ReflectiveCopy(ref object destination, ref object source, List<object> completed, Dictionary<object, object> equivs)
        {
            Type t = destination.GetType();
            // should value types just be naively copied?
            if (false)
                if (t.IsValueType)
                {
                    ValueType v2 = (ValueType) source;
                    destination = v2;
                    return true;
                }

            //promise the destination is going to be "completed" when we leave this function
            completed.Add(destination);
            //source is completed of course
            completed.Add(source);
            // for now on, whenever we see a destination we will use source ref
            equivs[destination] = source;

            foreach (FieldInfo fieldInfo in
                t.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public))
            {
                Type ft = fieldInfo.FieldType;

                object sourceValue = fieldInfo.GetValue(source);

                // should value types just be naively copied?
                if (false && ft.IsValueType)
                {
                    fieldInfo.SetValue(destination, sourceValue);
                    continue;
                }

                if (sourceValue == null)
                {
                    fieldInfo.SetValue(destination, null);
                    continue;
                }

                object targetObj = fieldInfo.GetValue(destination);

                object equivalent;
                if (equivs.TryGetValue(targetObj, out equivalent))
                {
                    //if (sourceValue != null) completed.Add(equivalent); // e1 will now be found in destination
                    fieldInfo.SetValue(destination, equivalent); // reuse object
                    continue;
                }

                // loop avoider
                if (completed.Contains(targetObj)) continue;
                if (targetObj == null)
                {
                    completed.Add(equivalent); // e1 will now be found in destination
                    fieldInfo.SetValue(destination, sourceValue); // reuse object
                    continue;
                }
                if (!ReflectiveCopy(ref targetObj, ref sourceValue, completed, equivs)) return false;
            }
            return true;
        }

        private object ToLispValue(Type type, object p)
        {
            throw new NotImplementedException();
        }

        public static object SerializeLisp(OSD osd)
        {
            switch (osd.Type)
            {
                case OSDType.Unknown:
                    throw new InvalidCastException();
                case OSDType.Boolean:
                    return osd.AsBoolean();
                case OSDType.Integer:
                    return osd.AsInteger();
                case OSDType.Real:
                    return osd.AsReal();
                case OSDType.String:
                    return osd.AsString();
                case OSDType.Date:
                    return osd.AsDate();
                case OSDType.URI:
                    return osd.AsUri();
                case OSDType.UUID:
                    return osd.AsUUID();

                case OSDType.Binary:
                    return osd.AsBinary();
                case OSDType.Array:
                    OSDArray args = (OSDArray) osd;
                    Cons ret = null;
                    for (int i = args.Count - 1; i >= 0; --i)
                    {
                        ret = new Cons(args[i], ret);
                    }
                    return ret;
                case OSDType.Map:
                    Cons list = null;
                    OSDMap map = (OSDMap) osd;
                    foreach (KeyValuePair<string, OSD> kvp in map)
                    {
                        Cons kv = new Cons(kvp.Key, new Cons(SerializeLisp(kvp.Value)));
                        list = new Cons(kv,list);
                    }
                    return Cons.Reverse(list);
                default:
                    return osd;
            }

        }


        public Primitive AddTempPrim(SimRegion R, string name, PrimType primType, Vector3 scale, Vector3 loc)
        {
            Primitive.ConstructionData CD = ObjectManager.BuildBasicShape(primType);
            CD.Material = Material.Light;
            CD.ProfileHole = HoleType.Triangle;

            bool success = false;

            Simulator simulator = R.TheSimulator;
            Primitive newPrim = null;
            // Register a handler for the creation event
            AutoResetEvent creationEvent = new AutoResetEvent(false);
            Quaternion rot = Quaternion.Identity;
            ObjectManager.NewPrimCallback callback =
                delegate(Simulator simulator0, Primitive prim, ulong regionHandle, ushort timeDilation)
                {
                    if (regionHandle != R.RegionHandle) return;
                    if ((loc - prim.Position).Length() > 3)
                    {
                        Debug("Not the prim " + (loc - prim.Position).Length());
                        return;
                    }
                    if (prim.PrimData.ProfileHole != HoleType.Triangle)
                    {
                        Debug("Not the prim?  prim.PrimData.ProfileHole != HoleType.Triangle: {0}!={1}",
                              prim.PrimData.ProfileHole, HoleType.Triangle);
                        // return;       //
                    }
                    if (Material.Light != prim.PrimData.Material)
                    {
                        Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light,
                              prim.PrimData.Material);
                        // return;
                    }
                    if ((prim.Flags & PrimFlags.CreateSelected) == 0)
                    {
                        Debug("Not the prim? (prim.Flags & PrimFlags.CreateSelected) == 0) was {0}", prim.Flags);
                        // return;
                    }
                    if (primType != prim.Type)
                    {
                        Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light,
                              prim.PrimData.Material);
                        // return;
                    }
                    //if (prim.Scale != scale) return;
                    //     if (prim.Rotation != rot) return;

                    //  if (Material.Light != prim.PrimData.Material) return;
                    //if (CD != prim.PrimData) return;
                    newPrim = prim;
                    creationEvent.Set();
                };

            client.Objects.OnNewPrim += callback;

            // Start the creation setting process (with baking enabled or disabled)
            client.Objects.AddPrim(simulator, CD, UUID.Zero, loc, scale, rot,
                                   PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);

            // Wait for the process to complete or time out
            if (creationEvent.WaitOne(1000 * 120, false))
                success = true;

            // Unregister the handler
            client.Objects.OnNewPrim -= callback;

            // Return success or failure message
            if (!success)
            {
                Debug("Timeout on new prim " + name);
                return null;
            }
            uint LocalID = newPrim.LocalID;
            client.Objects.SetName(simulator, LocalID, name);
            client.Objects.SetPosition(simulator, LocalID, loc);
            client.Objects.SetScale(simulator, LocalID, scale, true, true);
            client.Objects.SetRotation(simulator, LocalID, rot);
            client.Objects.SetFlags(LocalID, false, true, true, false);
            return newPrim;
        }

        public void SetObjectPosition(Primitive Prim, Vector3 localPos)
        {
            Simulator sim = GetSimulator(Prim);
            client.Objects.SetPosition(sim, Prim.LocalID, localPos);
        }

        public void SetObjectRotation(Primitive Prim, Quaternion localPos)
        {
            Simulator sim = GetSimulator(Prim);
            client.Objects.SetRotation(sim, Prim.LocalID, localPos);
        }

        public SimWaypoint GetWaypoint(Vector3d gloabl)
        {
            return SimRegion.GetWaypoint(gloabl);
        }

        public void DeletePrim(Primitive thePrim)
        {
            if (thePrim is Avatar) return;
            SimObject O = GetSimObject(thePrim);
            if (O != null)
            {
                SimObjects.Remove(O);
                SendOnRemoveSimObject(O);
            }
            uint objectLocalID = thePrim.LocalID;
            client.Inventory.RequestDeRezToInventory(objectLocalID, DeRezDestination.AgentInventoryTake,
                                                     client.Inventory.FindFolderForType(AssetType.TrashFolder),
                                                     UUID.Random());
        }

    }
}
