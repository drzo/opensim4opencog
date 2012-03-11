using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;
using cogbot.Listeners;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public class Linkset
    {
        public ImportCommand.PrimToCreate Parent;
        public List<ImportCommand.PrimToCreate> Children = new List<ImportCommand.PrimToCreate>();
    }

    public partial class OarFile
    {
        enum ProfileShape0 : byte
        {
            Circle = 0,
            Square = 1,
            IsometricTriangle = 2,
            EquilateralTriangle = 3,
            RightTriangle = 4,
            HalfCircle = 5
        }

        public static void SavePrims(DoubleDictionary<uint, UUID, Primitive> prims, string path, ImportSettings options)
        {
            // Delete all of the old linkset files
            try
            {
                Directory.Delete(path, true);
                Directory.CreateDirectory(path);
            }
            catch (Exception ex)
            {
                Logger.Log("Failed saving prims: " + ex.Message, Helpers.LogLevel.Error);
                return;
            }

            // Copy the double dictionary to a temporary list for iterating
            List<Primitive> primList = new List<Primitive>();
            prims.ForEach(delegate(Primitive prim)
            {
                primList.Add(prim);
            });

            foreach (Primitive p in primList)
            {
                if (p.ParentID == 0)
                {
                    Linkset linkset = new Linkset();
                    linkset.Parent = ImportCommand.Running.APrimToCreate(p);

                    prims.ForEach(delegate(Primitive q)
                    {
                        if (q.ParentID == p.LocalID)
                            linkset.Children.Add(ImportCommand.Running.APrimToCreate(q));
                    });

                    SaveLinkset(linkset, path + "/Primitive_" + linkset.Parent.NewID.ToString() + ".xml", options);
                }
            }
        }

        static public void SaveLinkset(Linkset linkset, string filename, ImportSettings options)
        {
            try
            {
                using (StreamWriter stream = new StreamWriter(filename))
                {
                    XmlTextWriter writer = new XmlTextWriter(stream);
                    SOGToXml2(writer, linkset, options);
                    writer.Flush();
                }
            }
            catch (Exception ex)
            {
                Logger.Log("Failed saving linkset: " + ex.Message, Helpers.LogLevel.Error);
            }
        }

        static void SOGToXml2(XmlTextWriter writer, Linkset linkset, ImportSettings options)
        {
            writer.WriteStartElement(String.Empty, "SceneObjectGroup", String.Empty);
            SOPToXml(writer, linkset.Parent.Prim, 0, null, GetTaskInv(linkset.Parent), options);
            writer.WriteStartElement(String.Empty, "OtherParts", String.Empty);

            //uint linkNum = sop.LocalID - parent.LocalID;
            int linkNum = 1;
            foreach (var child in linkset.Children)
            {
                SOPToXml(writer, child.Prim, linkNum++, linkset.Parent.Prim, GetTaskInv(child), options);
            }

            writer.WriteEndElement();
            writer.WriteEndElement();
        }

        private static ICollection<InventoryBase> GetTaskInv(ImportCommand.PrimToCreate primitive)
        {
            //todo
            return primitive.TaskInventory;
        }

        // public static void SOPToXml2(XmlTextWriter writer, Primitive sop, ImportSettings options)
        static void SOPToXml(XmlTextWriter writer, Primitive sop, int linkNum, Primitive parent, ICollection<InventoryBase> taskInventory, ImportSettings options)
        {
            // Primitive parent = null; throw new NotImplementedException();//"GETPArent"
            Primitive.ObjectProperties prop = sop.Properties;
            writer.WriteStartElement("SceneObjectPart");
            writer.WriteAttributeString("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
            writer.WriteAttributeString("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");

            WriteValue(writer,"AllowedDrop",
                                      ((sop.Flags & PrimFlags.AllowInventoryDrop) != 0).ToString().ToLower());

            WriteUUID(writer, "CreatorID", prop.CreatorID, options);
            /*
            if (sop.CreatorData != null && sop.CreatorData != string.Empty)
                WES(writer,"CreatorData", sop.CreatorData);
            else if (options.ContainsKey("home"))
            {
                if (m_UserManagement == null)
                    m_UserManagement = sop.ParentGroup.Scene.RequestModuleInterface<IUserManagement>();
                string name = m_UserManagement.GetUserName(sop.CreatorID);
                WES(writer,"CreatorData", (string)options["home"] + ";" + name);
            }
            */
            WriteUUID(writer, "FolderID", prop.FolderID, options);
            WriteInt(writer,"InventorySerial", prop.InventorySerial);

            WriteTaskInventory(writer, taskInventory, options);

            WriteUUID(writer, "UUID", sop.ID, options);
            WriteFlags(writer, "ObjectFlags", sop.Flags, options);
            WriteInt(writer,"LocalId", sop.LocalID);
            WriteValue(writer,"Name", prop.Name);
            WriteEnum(writer, "Material", sop.PrimData.Material);
            WriteValue(writer,"PassTouches", "False");// sop.PassTouches.ToString().ToLower());
            WriteUInt(writer,"RegionHandle", sop.RegionHandle);
            WriteValue(writer,"ScriptAccessPin", "0");

            Vector3 groupPosition;
            if (parent == null)
                groupPosition = sop.Position;
            else
                groupPosition = parent.Position;

            WriteVector(writer, "GroupPosition", groupPosition);
            if (sop.ParentID == 0)
                WriteVector(writer, "OffsetPosition", Vector3.Zero);
            else
                WriteVector(writer, "OffsetPosition", sop.Position);
            WriteQuaternion(writer, "RotationOffset", sop.Rotation);

            WriteVector(writer, "Velocity", sop.Velocity);
            WriteVector(writer, "AngularVelocity", sop.AngularVelocity);
            WriteVector(writer, "Acceleration", sop.Acceleration);
            WriteValue(writer,"Description", prop.Description);

            writer.WriteStartElement("Color");
            WriteFloat(writer,"R", sop.TextColor.R);
            WriteFloat(writer,"G", sop.TextColor.G);
            WriteFloat(writer,"B", sop.TextColor.B);
            WriteFloat(writer,"A", sop.TextColor.G);
            writer.WriteEndElement();

            WriteValue(writer,"Text", sop.Text);
            WriteValue(writer,"SitName", prop.SitName);
            WriteValue(writer,"TouchName", prop.TouchName);

            WriteInt(writer,"LinkNum", linkNum);
            WriteEnum(writer,"ClickAction", sop.ClickAction);

            WriteShape(writer, sop, sop.Properties, sop.PrimData, options);

            WriteVector(writer, "Scale", sop.Scale);
            WriteValue(writer,"UpdateFlag", "0");
            WriteVector(writer, "SitTargetOrientation", Vector3.UnitZ);
            WriteVector(writer, "SitTargetPosition", Vector3.Zero);
            WriteVector(writer, "SitTargetPositionLL", Vector3.Zero);
            WriteQuaternion(writer, "SitTargetOrientationLL", new Quaternion(0f, 0f, 1f, 0f));
            WriteInt(writer,"ParentID", sop.ParentID);
            WriteDate(writer, "CreationDate", sop.Properties.CreationDate);
            WriteEnum(writer, "Category", sop.Properties.Category);
            WriteInt(writer,"SalePrice", sop.Properties.SalePrice);
            WriteEnum(writer, "ObjectSaleType", sop.Properties.SaleType);
            WriteInt(writer,"OwnershipCost", sop.Properties.OwnershipCost);
            WriteUUID(writer, "GroupID", sop.GroupID, options);

            UUID ownerID = options.ContainsKey("wipe-owners") ? UUID.Zero : sop.OwnerID;
            WriteUUID(writer, "OwnerID", ownerID, options);

            UUID lastOwnerID = options.ContainsKey("wipe-owners") ? UUID.Zero : prop.LastOwnerID;
            WriteUUID(writer, "LastOwnerID", lastOwnerID, options);

            var perms = prop.Permissions;
            WriteEnum(writer, "BaseMask", perms.BaseMask);
            WriteEnum(writer, "OwnerMask", perms.OwnerMask);
            WriteEnum(writer, "GroupMask", perms.GroupMask);
            WriteEnum(writer, "EveryoneMask", perms.EveryoneMask);
            WriteEnum(writer, "NextOwnerMask", perms.NextOwnerMask);
            WriteFlags(writer, "Flags", sop.Flags, options);
            WriteUUID(writer, "CollisionSound", sop.Sound, options);
            WriteFloat(writer,"CollisionSoundVolume", sop.SoundGain);
            if (sop.MediaURL != null)
                WriteValue(writer,"MediaUrl", sop.MediaURL);
            WriteBytes(writer, "TextureAnimation", sop.TextureAnim.GetBytes());
            WriteBytes(writer, "ParticleSystem", sop.ParticleSys.GetBytes());
            /*
            WES(writer,"PayPrice0", sop.PayPrice[0]);
            WES(writer,"PayPrice1", sop.PayPrice[1]);
            WES(writer,"PayPrice2", sop.PayPrice[2]);
            WES(writer,"PayPrice3", sop.PayPrice[3]);
            WES(writer,"PayPrice4", sop.PayPrice[4]);
            */

            writer.WriteEndElement();
        }
        public static void WriteTaskInventory(XmlTextWriter writer, ICollection<InventoryBase> tinv, ImportSettings options)
        {
            if (tinv == null) return;
            if (tinv.Count > 0) // otherwise skip this
            {
                writer.WriteStartElement("TaskInventory");

                foreach (InventoryBase item2c in tinv)
                {
                    InventoryItem item = item2c as InventoryItem;
                    if (item == null) continue;                   
                    writer.WriteStartElement("TaskInventoryItem");
                    Permissions perms = item.Permissions;

                    if (CogbotHelpers.IsNullOrZero(item.AssetUUID))
                    {
                        ImportCommand.Running.Failure("Zero AssetID " + item.Name);
                    }
                    WriteUUID(writer, "AssetID", item.AssetUUID, options);
                    WriteEnum(writer, "BasePermissions", perms.BaseMask);
                    WriteDate(writer, "CreationDate", item.CreationDate);

                    WriteUUID(writer, "CreatorID", item.CreatorID, options);

                    /*
                    //todo if (item.CreatorData != null && item.CreatorData != string.Empty)
                        WES(writer,"CreatorData", item.CreatorData);
                    else if (options.ContainsKey("home"))
                    {
                        if (m_UserManagement == null)
                            m_UserManagement = scene.RequestModuleInterface<IUserManagement>();
                        string name = m_UserManagement.GetUserName(item.CreatorID);
                        WES(writer,"CreatorData", (string)options["home"] + ";" + name);
                    }
                    */
                    WriteValue(writer,"Description", item.Description);
                    WriteEnum(writer, "EveryonePermissions", perms.EveryoneMask);
                    WriteInt(writer,"Flags", item.Flags);
                    WriteUUID(writer, "GroupID", item.GroupID, options);
                    WriteEnum(writer, "GroupPermissions", perms.GroupMask);
                    WriteEnum(writer, "InvType", item.InventoryType);
                    WriteUUID(writer, "ItemID", item.UUID, options);
                    //todo WriteUUID(writer, "OldItemID", item.OldItemID, options);

                    UUID lastOwnerID = options.ContainsKey("wipe-owners") ? UUID.Zero : item.LastOwnerID;
                    WriteUUID(writer, "LastOwnerID", lastOwnerID, options);

                    WriteValue(writer,"Name", item.Name);
                    WriteEnum(writer, "NextPermissions", perms.NextOwnerMask);

                    UUID ownerID = options.ContainsKey("wipe-owners") ? UUID.Zero : item.OwnerID;
                    WriteUUID(writer, "OwnerID", ownerID, options);

                    WriteEnum(writer, "CurrentPermissions", perms.OwnerMask);
                    WriteUUID(writer, "ParentID", item.ParentUUID, options);
                    /*todo
                     * WriteUUID(writer, "ParentPartID", item.ParentPartID, options);
                     WriteUUID(writer, "PermsGranter", item.PermsGranter, options);
                     */
                    // todo what is this?
                    WriteEnum(writer, "PermsMask", PermissionMask.All);//  perms.BaseMask);//item.PermsMask);

                    WriteEnum(writer, "Type", item.AssetType);
                    //todo WES(writer,"OwnerChanged", item.OwnerChanged.ToString().ToLower());

                    writer.WriteEndElement(); // TaskInventoryItem
                }

                writer.WriteEndElement(); // TaskInventory
            }
        }
        public static void WriteShape(XmlTextWriter writer, Primitive prim, Primitive.ObjectProperties props, Primitive.ConstructionData shp, ImportSettings options)
        {
            if (true /*shp != null*/)
            {
                writer.WriteStartElement("Shape");

                WriteEnum(writer, "ProfileCurve", shp.ProfileCurve);

                writer.WriteStartElement("TextureEntry");
                byte[] te;
                if (prim.Textures != null)
                    te = prim.Textures.GetBytes();
                else
                    te = Utils.EmptyBytes;
                writer.WriteBase64(te, 0, te.Length);
                writer.WriteEndElement(); // TextureEntry

                writer.WriteStartElement("ExtraParams");
                byte[] ep;
                if (prim.GetExtraParamsBytes() != null)
                    ep = prim.GetExtraParamsBytes();
                else
                    ep = Utils.EmptyBytes;
                writer.WriteBase64(ep, 0, ep.Length);
                writer.WriteEndElement(); // ExtraParams

                writer.WriteElementString("PathBegin", Primitive.PackBeginCut(shp.PathBegin).ToString());
                WriteEnum(writer, "PathCurve", shp.PathCurve);
                writer.WriteElementString("PathEnd", Primitive.PackEndCut(shp.PathEnd).ToString());
                writer.WriteElementString("PathRadiusOffset", Primitive.PackPathTwist(shp.PathRadiusOffset).ToString());
                writer.WriteElementString("PathRevolutions", Primitive.PackPathRevolutions(shp.PathRevolutions).ToString());
                writer.WriteElementString("PathScaleX", Primitive.PackPathScale(shp.PathScaleX).ToString());
                writer.WriteElementString("PathScaleY", Primitive.PackPathScale(shp.PathScaleY).ToString());
                writer.WriteElementString("PathShearX", ((byte)Primitive.PackPathShear(shp.PathShearX)).ToString());
                writer.WriteElementString("PathShearY", ((byte)Primitive.PackPathShear(shp.PathShearY)).ToString());
                writer.WriteElementString("PathSkew", Primitive.PackPathTwist(shp.PathSkew).ToString());
                writer.WriteElementString("PathTaperX", Primitive.PackPathTaper(shp.PathTaperX).ToString());
                writer.WriteElementString("PathTaperY", Primitive.PackPathTaper(shp.PathTaperY).ToString());
                writer.WriteElementString("PathTwist", Primitive.PackPathTwist(shp.PathTwist).ToString());
                writer.WriteElementString("PathTwistBegin", Primitive.PackPathTwist(shp.PathTwistBegin).ToString());
                writer.WriteElementString("ProfileBegin", Primitive.PackBeginCut(shp.ProfileBegin).ToString());
                writer.WriteElementString("ProfileEnd", Primitive.PackEndCut(shp.ProfileEnd).ToString());
                writer.WriteElementString("ProfileHollow", Primitive.PackProfileHollow(shp.ProfileHollow).ToString()); 
                
                WriteEnum(writer, "PCode", shp.PCode);
                WriteInt(writer,"State", shp.State);

                WriteFlags(writer, "ProfileShape", shp.ProfileCurve, options);
                WriteFlags(writer, "HollowShape", shp.ProfileHole, options);

                var Sculpt = prim.Sculpt;
                if (Sculpt != null)
                {
                    WriteUUID(writer, "SculptTexture", Sculpt.SculptTexture, options);
                    WriteEnum(writer, "SculptType", Sculpt.Type);
                    writer.WriteStartElement("SculptData");
                    byte[] sd;
                    if (prim.Sculpt != null)
                        sd = Sculpt.GetBytes();
                    else
                        sd = Utils.EmptyBytes;
                    writer.WriteBase64(sd, 0, sd.Length);
                    writer.WriteEndElement(); // SculptData
                }

                Primitive.FlexibleData Flexi = prim.Flexible;
                if (Flexi != null)
                {
                    WriteInt(writer,"FlexiSoftness", Flexi.Softness);
                    WriteFloat(writer,"FlexiTension", Flexi.Tension);
                    WriteFloat(writer,"FlexiDrag", Flexi.Drag);
                    WriteFloat(writer,"FlexiGravity", Flexi.Gravity);
                    WriteFloat(writer,"FlexiWind", Flexi.Wind);
                    WriteFloat(writer,"FlexiForceX", Flexi.Force.X);
                    WriteFloat(writer,"FlexiForceY", Flexi.Force.Y);
                    WriteFloat(writer,"FlexiForceZ", Flexi.Force.Z);
                }

                Primitive.LightData Light = prim.Light;
                if (Light != null)
                {
                    WriteFloat(writer,"LightColorR", Light.Color.R);
                    WriteFloat(writer,"LightColorG", Light.Color.G);
                    WriteFloat(writer,"LightColorB", Light.Color.B);
                    WriteFloat(writer,"LightColorA", Light.Color.A);
                    WriteFloat(writer,"LightRadius", Light.Radius);
                    WriteFloat(writer,"LightCutoff", Light.Cutoff);
                    WriteFloat(writer,"LightFalloff", Light.Falloff);
                    WriteFloat(writer,"LightIntensity", Light.Intensity);
                }

                WriteValue(writer,"FlexiEntry", (Flexi != null).ToString().ToLower());
                WriteValue(writer,"LightEntry", (Light != null).ToString().ToLower());
                WriteValue(writer,"SculptEntry", (Sculpt != null).ToString().ToLower());

                //todo if (shp.Media != null) WES(writer,"Media", shp.Media.ToXml());

                writer.WriteEndElement(); // Shape
            }
        }
    }
}
