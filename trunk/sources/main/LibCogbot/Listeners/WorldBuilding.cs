using System;
using System.Collections.Generic;
using System.Threading;
using System.Xml.Serialization;
using DotLisp;
using MushDLR223.Utilities;
using OpenMetaverse.StructuredData;
using cogbot.TheOpenSims;
using OpenMetaverse;
using System.Reflection;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{

    public partial class WorldObjects
    {
        static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes = new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();

        private static KeyValuePair<List<PropertyInfo>, List<FieldInfo>> GetPropsForTypes(Type t)
        {
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> kv;

            if (PropForTypes.TryGetValue(t, out kv)) return kv;

            lock (PropForTypes)
            {
                if (!PropForTypes.TryGetValue(t, out kv))
                {
                    kv = new KeyValuePair<List<PropertyInfo>, List<FieldInfo>>(new List<PropertyInfo>(),
                                                                               new List<FieldInfo>());
                    var ta = t.GetCustomAttributes(typeof (XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute) ta[0];
                        specialXMLType = true;
                    }
                    HashSet<string> lowerProps = new HashSet<string>();
                    BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
                    foreach (
                        PropertyInfo o in t.GetProperties(flags))
                    {
                        if (o.CanRead)
                        {

                            if (o.Name.StartsWith("_")) continue;
                            if (o.DeclaringType == typeof (Object)) continue;
                            if (!lowerProps.Add(o.Name.ToLower())) continue;
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof (Object)) continue;
                        if (!lowerProps.Add(o.Name.ToLower())) continue;
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                            if (use == null || use.Length < 1) continue;
                        }
                        kv.Value.Add(o);
                    }
                }
                return kv;
            }
        }

        public static List<NamedParam> GetMemberValues(string prefix, Object properties)
        {
            List<NamedParam> dict = new List<NamedParam>();
            if (properties == null)
            {
                return dict;
            }
            Type t = properties.GetType();
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> vvv = GetPropsForTypes(t);
            HashSet<string> lowerProps = new HashSet<string>();
            BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
            foreach (
                PropertyInfo o in vvv.Key)
            {
                {
                    try
                    {
                        var v = o.GetValue(properties, null);
                        if (v == null)
                        {
                            v = new NullType(properties, o);
                        }
                        else if (o.PropertyType == typeof(UUID))
                        {
                            GetUUIDType(o, (UUID)v);
                        }
                        dict.Add(new NamedParam(properties, o, prefix + o.Name, o.PropertyType, v));
                    }
                    catch (Exception e)
                    {
                        DLRConsole.DebugWriteLine("" + e);
                    }
                }
            }
            foreach (FieldInfo o in vvv.Value)
            {
                try
                {
                    var v = o.GetValue(properties);
                    if (v == null)
                    {
                        v = new NullType(properties, o);
                    }
                    else if (o.FieldType == typeof (UUID))
                    {
                        GetUUIDType(o, (UUID) v);
                    }
                    dict.Add(new NamedParam(properties, o, prefix + o.Name, o.FieldType, v));
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
            return dict;
        }

        delegate void UUIDGleaner(UUID uuid, Simulator sim);

        //private static Dictionary<string, Action<UUID>> UUID2Memeber = new Dictionary<string, Action<UUID>>();
        private static void GetUUIDType(MemberInfo info, UUID o)
        {
            if (o == UUID.Zero) return;
            UUIDGleaner act = GetUUIDType(info.Name);
            act(o, null);
        }
                

        private static readonly Dictionary<string, UUIDGleaner> UUID2Type = new Dictionary<string, UUIDGleaner>();
        static UUIDGleaner GetUUIDType(string p)
        {

            lock (UUID2Type)
                if (UUID2Type.Count == 0)
                {
                    UUIDGleaner texture = ((obj, sim) => { SimAssetStore.FindOrCreateAsset(obj, AssetType.Texture); });
                    UUIDGleaner avatar = ((obj, sim) => { GridMaster.DeclareAvatarProfile(obj); });
                    UUIDGleaner task = ((obj, sim) => { GridMaster.DeclareTask(obj, sim); });
                    UUIDGleaner nothing = ((obj, sim) => { });
                    UUIDGleaner role = ((obj, sim) => DeclareGeneric("GroupRole", obj, " +p")); 
                    UUID2Type[""] = nothing;
                    UUID2Type["ID"] = nothing;
                    UUID2Type["Sound"] = ((obj, sim) => { SimAssetStore.FindOrCreateAsset(obj, AssetType.Sound); });
                    UUID2Type["Image"]
                        = UUID2Type["SculptTexture"]
                          = UUID2Type["Photo"]
                            = UUID2Type["Insignia"]
                              = UUID2Type["Picture"]
                                = UUID2Type["Texture"]
                                  = UUID2Type["Sculpt"]
                                    = UUID2Type["ProfileImage"] = texture;
                    UUID2Type["Partner"] = UUID2Type["Creator"] = UUID2Type["Founder"] = avatar;
                    UUID2Type["Group"] = ((obj, sim) => { GridMaster.DeclareGroup(obj); });
                    UUID2Type["Object"] = ((obj, sim) => { GridMaster.CreateSimObject(obj, GridMaster, sim); });
                    // todo inventory item 
                    UUID2Type["OwnerRole"] = role;
                    UUID2Type["FromTask"] = task;
                    UUID2Type["FolderID"] = nothing;
                    UUID2Type["ItemID"] = nothing;
                    UUID2Type["OwnerID"] = avatar;
                    UUID2Type["Owner"] = avatar;
                    // ussualyl objects but need to confirm: The Key of the specified target object or avatar particles will follow" 
                    UUID2Type["Target"] = nothing;
                    UUID2Type["Asset"] = nothing;
                }
            UUIDGleaner o;
            lock (UUID2Type)
            {
                if (UUID2Type.TryGetValue(p, out o)) return o;

                if (p.StartsWith("Next"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(4));
                }
                if (p.StartsWith("Last"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(4));
                }
                if (p.StartsWith("Life"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(4));
                }
                if (p.StartsWith("First"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(5));
                }
                if (p.StartsWith("Second"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(6));
                }
                if (p.EndsWith("ID"))
                {
                    return UUID2Type[p] = GetUUIDType(p.Substring(0, p.Length - 2));
                }
                Debug("Dont know what UUID means in " + p);
                return UUID2Type[p] = SkipUUID;
            }
        }

        private static void SkipUUID(UUID obj, Simulator sim)
        {           
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
                //obj.Properties = null;
                obj.Properties = obj.Properties;
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
            EventHandler<PrimEventArgs> callback =
                (s,e)=>//delegate(Simulator simulator0, Primitive prim, ulong regionHandle, ushort timeDilation)
                {
                    var regionHandle = e.Simulator.Handle;
                    var prim = e.Prim;
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

            client.Objects.ObjectUpdate += callback;

            // Start the creation setting process (with baking enabled or disabled)
            client.Objects.AddPrim(simulator, CD, UUID.Zero, loc, scale, rot,
                                   PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);

            // Wait for the process to complete or time out
            if (creationEvent.WaitOne(1000 * 120, false))
                success = true;

            // Unregister the handler
            client.Objects.ObjectUpdate -= callback;

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
            client.Objects.SetFlags(simulator, LocalID, false, true, true, false);
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
