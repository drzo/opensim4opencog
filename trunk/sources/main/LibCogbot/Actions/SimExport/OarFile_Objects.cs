using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public class Linkset
    {
        public Primitive Parent;
        public List<Primitive> Children = new List<Primitive>();
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
                    linkset.Parent = p;

                    prims.ForEach(delegate(Primitive q)
                    {
                        if (q.ParentID == p.LocalID)
                            linkset.Children.Add(q);
                    });

                    SaveLinkset(linkset, path + "/Primitive_" + linkset.Parent.ID.ToString() + ".xml", options);
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
            SOPToXml(writer, linkset.Parent, 0, null, GetTaskInv(linkset.Parent), options);
            writer.WriteStartElement(String.Empty, "OtherParts", String.Empty);

            //uint linkNum = sop.LocalID - parent.LocalID;
            int linkNum = 1;
            foreach (Primitive child in linkset.Children)
            {
                SOPToXml(writer, child, linkNum++, linkset.Parent, GetTaskInv(child), options);
            }

            writer.WriteEndElement();
            writer.WriteEndElement();
        }

        private static ICollection<InventoryItem> GetTaskInv(Primitive primitive)
        {
            //todo
            throw new global::System.NotImplementedException();
        }

        // public static void SOPToXml2(XmlTextWriter writer, Primitive sop, ImportSettings options)
        static void SOPToXml(XmlTextWriter writer, Primitive sop, int linkNum, Primitive parent, ICollection<InventoryItem> taskInventory, ImportSettings options)
        {
            // Primitive parent = null; throw new NotImplementedException();//"GETPArent"
            Primitive.ObjectProperties prop = sop.Properties;
            writer.WriteStartElement("SceneObjectPart");
            writer.WriteAttributeString("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
            writer.WriteAttributeString("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");

            writer.WriteElementString("AllowedDrop",
                                      ((sop.Flags & PrimFlags.AllowInventoryDrop) != 0).ToString().ToLower());

            WriteUUID(writer, "CreatorID", prop.CreatorID, options);
            /*
            if (sop.CreatorData != null && sop.CreatorData != string.Empty)
                writer.WriteElementString("CreatorData", sop.CreatorData);
            else if (options.ContainsKey("home"))
            {
                if (m_UserManagement == null)
                    m_UserManagement = sop.ParentGroup.Scene.RequestModuleInterface<IUserManagement>();
                string name = m_UserManagement.GetUserName(sop.CreatorID);
                writer.WriteElementString("CreatorData", (string)options["home"] + ";" + name);
            }
            */
            WriteUUID(writer, "FolderID", prop.FolderID, options);
            writer.WriteElementString("InventorySerial", prop.InventorySerial.ToString());

            WriteTaskInventory(writer, taskInventory, options);

            WriteUUID(writer, "UUID", sop.ID, options);
            writer.WriteElementString("ObjectFlags", ((int)sop.Flags).ToString());
            writer.WriteElementString("LocalId", sop.LocalID.ToString());
            writer.WriteElementString("Name", prop.Name);
            writer.WriteElementString("Material", sop.PrimData.Material.ToString());
            writer.WriteElementString("PassTouches", "0");// sop.PassTouches.ToString().ToLower());
            writer.WriteElementString("RegionHandle", sop.RegionHandle.ToString());
            writer.WriteElementString("ScriptAccessPin", "0");

            Vector3 groupPosition;
            if (parent == null)
                groupPosition = sop.Position;
            else
                groupPosition = parent.Position;

            WriteVector(writer, "GroupPosition", groupPosition);
            WriteVector(writer, "OffsetPosition", groupPosition - sop.Position);

            WriteQuaternion(writer, "RotationOffset", sop.Rotation);
            WriteVector(writer, "Velocity", sop.Velocity);
            WriteVector(writer, "AngularVelocity", sop.AngularVelocity);
            WriteVector(writer, "Acceleration", sop.Acceleration);
            writer.WriteElementString("Description", prop.Description);

            writer.WriteStartElement("Color");
            writer.WriteElementString("R", sop.TextColor.R.ToString());
            writer.WriteElementString("G", sop.TextColor.G.ToString());
            writer.WriteElementString("B", sop.TextColor.B.ToString());
            writer.WriteElementString("A", sop.TextColor.G.ToString());
            writer.WriteEndElement();

            writer.WriteElementString("Text", sop.Text);
            writer.WriteElementString("SitName", prop.SitName);
            writer.WriteElementString("TouchName", prop.TouchName);

            writer.WriteElementString("LinkNum", linkNum.ToString());
            writer.WriteElementString("ClickAction", sop.ClickAction.ToString());

            WriteShape(writer, sop, sop.Properties, sop.PrimData, options);

            WriteVector(writer, "Scale", sop.Scale);
            writer.WriteElementString("UpdateFlag", "0");
            WriteVector(writer, "SitTargetOrientation", Vector3.UnitZ);
            WriteVector(writer, "SitTargetPosition", Vector3.Zero);
            WriteVector(writer, "SitTargetPositionLL", Vector3.Zero);
            WriteQuaternion(writer, "SitTargetOrientationLL", new Quaternion(0f, 0f, 1f, 0f));
            writer.WriteElementString("ParentID", sop.ParentID.ToString());
            writer.WriteElementString("CreationDate", ((int)Utils.DateTimeToUnixTime(sop.Properties.CreationDate)).ToString());
            writer.WriteElementString("Category", ((int)sop.Properties.Category).ToString());
            writer.WriteElementString("SalePrice", sop.Properties.SalePrice.ToString());
            writer.WriteElementString("ObjectSaleType", ((int)sop.Properties.SaleType).ToString());
            writer.WriteElementString("OwnershipCost", sop.Properties.OwnershipCost.ToString());
            WriteUUID(writer, "GroupID", sop.GroupID, options);

            UUID ownerID = options.ContainsKey("wipe-owners") ? UUID.Zero : sop.OwnerID;
            WriteUUID(writer, "OwnerID", ownerID, options);

            UUID lastOwnerID = options.ContainsKey("wipe-owners") ? UUID.Zero : prop.LastOwnerID;
            WriteUUID(writer, "LastOwnerID", lastOwnerID, options);

            var perms = prop.Permissions;
            writer.WriteElementString("BaseMask", perms.BaseMask.ToString());
            writer.WriteElementString("OwnerMask", perms.OwnerMask.ToString());
            writer.WriteElementString("GroupMask", perms.GroupMask.ToString());
            writer.WriteElementString("EveryoneMask", perms.EveryoneMask.ToString());
            writer.WriteElementString("NextOwnerMask", perms.NextOwnerMask.ToString());
            WriteFlags(writer, "Flags", sop.Flags.ToString(), options);
            WriteUUID(writer, "CollisionSound", sop.Sound, options);
            writer.WriteElementString("CollisionSoundVolume", sop.SoundGain.ToString());
            if (sop.MediaURL != null)
                writer.WriteElementString("MediaUrl", sop.MediaURL.ToString());
            WriteBytes(writer, "TextureAnimation", sop.TextureAnim.GetBytes());
            WriteBytes(writer, "ParticleSystem", sop.ParticleSys.GetBytes());
            /*
            writer.WriteElementString("PayPrice0", sop.PayPrice[0].ToString());
            writer.WriteElementString("PayPrice1", sop.PayPrice[1].ToString());
            writer.WriteElementString("PayPrice2", sop.PayPrice[2].ToString());
            writer.WriteElementString("PayPrice3", sop.PayPrice[3].ToString());
            writer.WriteElementString("PayPrice4", sop.PayPrice[4].ToString());
            */

            writer.WriteEndElement();
        }
        public static void WriteTaskInventory(XmlTextWriter writer, ICollection<InventoryItem> tinv, ImportSettings options)
        {
            if (tinv.Count > 0) // otherwise skip this
            {
                writer.WriteStartElement("TaskInventory");

                foreach (InventoryItem item in tinv)
                {
                    writer.WriteStartElement("TaskInventoryItem");
                    Permissions perms = item.Permissions;

                    WriteUUID(writer, "AssetID", item.AssetUUID, options);
                    writer.WriteElementString("BasePermissions", perms.BaseMask.ToString());
                    writer.WriteElementString("CreationDate", item.CreationDate.ToString());

                    WriteUUID(writer, "CreatorID", item.CreatorID, options);

                    /*
                    //todo if (item.CreatorData != null && item.CreatorData != string.Empty)
                        writer.WriteElementString("CreatorData", item.CreatorData);
                    else if (options.ContainsKey("home"))
                    {
                        if (m_UserManagement == null)
                            m_UserManagement = scene.RequestModuleInterface<IUserManagement>();
                        string name = m_UserManagement.GetUserName(item.CreatorID);
                        writer.WriteElementString("CreatorData", (string)options["home"] + ";" + name);
                    }
                    */
                    writer.WriteElementString("Description", item.Description);
                    writer.WriteElementString("EveryonePermissions", perms.EveryoneMask.ToString());
                    writer.WriteElementString("Flags", item.Flags.ToString());
                    WriteUUID(writer, "GroupID", item.GroupID, options);
                    writer.WriteElementString("GroupPermissions", perms.GroupMask.ToString());
                    writer.WriteElementString("InvType", item.InventoryType.ToString());
                    WriteUUID(writer, "ItemID", item.UUID, options);
                    //todo WriteUUID(writer, "OldItemID", item.OldItemID, options);

                    UUID lastOwnerID = options.ContainsKey("wipe-owners") ? UUID.Zero : item.LastOwnerID;
                    WriteUUID(writer, "LastOwnerID", lastOwnerID, options);

                    writer.WriteElementString("Name", item.Name);
                    writer.WriteElementString("NextPermissions", perms.NextOwnerMask.ToString());

                    UUID ownerID = options.ContainsKey("wipe-owners") ? UUID.Zero : item.OwnerID;
                    WriteUUID(writer, "OwnerID", ownerID, options);

                    writer.WriteElementString("CurrentPermissions", perms.OwnerMask.ToString());
                    WriteUUID(writer, "ParentID", item.ParentUUID, options);
                    /*todo
                     * WriteUUID(writer, "ParentPartID", item.ParentPartID, options);
                     WriteUUID(writer, "PermsGranter", item.PermsGranter, options);
                     */
                    writer.WriteElementString("PermsMask", perms.BaseMask.ToString());//item.PermsMask.ToString());

                    writer.WriteElementString("Type", ((uint)item.AssetType).ToString());
                    //todo writer.WriteElementString("OwnerChanged", item.OwnerChanged.ToString().ToLower());

                    writer.WriteEndElement(); // TaskInventoryItem
                }

                writer.WriteEndElement(); // TaskInventory
            }
        }
        static void SOPToXmlz(XmlTextWriter writer, Primitive sop, Primitive parent, ImportSettings options)
        {
            writer.WriteStartElement("SceneObjectPart");
            writer.WriteAttributeString("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
            writer.WriteAttributeString("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");

            WriteUUID(writer, "CreatorID", sop.Properties.CreatorID);
            WriteUUID(writer, "FolderID", sop.Properties.FolderID);
            writer.WriteElementString("InventorySerial", sop.Properties.InventorySerial.ToString());
            writer.WriteStartElement("TaskInventory");
            writer.WriteEndElement();
            writer.WriteElementString("ObjectFlags", ((int)sop.Flags).ToString());
            WriteUUID(writer, "UUID", sop.ID);
            writer.WriteElementString("LocalId", sop.LocalID.ToString());
            writer.WriteElementString("Name", sop.Properties.Name);
            writer.WriteElementString("Material", ((int)sop.PrimData.Material).ToString());
            writer.WriteElementString("RegionHandle", sop.RegionHandle.ToString());
            writer.WriteElementString("ScriptAccessPin", "0");

            Vector3 groupPosition;
            if (parent == null)
                groupPosition = sop.Position;
            else
                groupPosition = parent.Position;

            WriteVector(writer, "GroupPosition", groupPosition);
            WriteVector(writer, "OffsetPosition", groupPosition - sop.Position);
            WriteQuaternion(writer, "RotationOffset", sop.Rotation);
            WriteVector(writer, "Velocity", Vector3.Zero);
            WriteVector(writer, "RotationalVelocity", Vector3.Zero);
            WriteVector(writer, "AngularVelocity", sop.AngularVelocity);
            WriteVector(writer, "Acceleration", Vector3.Zero);
            writer.WriteElementString("Description", sop.Properties.Description);
            writer.WriteStartElement("Color");
            writer.WriteElementString("R", sop.TextColor.R.ToString());
            writer.WriteElementString("G", sop.TextColor.G.ToString());
            writer.WriteElementString("B", sop.TextColor.B.ToString());
            writer.WriteElementString("A", sop.TextColor.G.ToString());
            writer.WriteEndElement();
            writer.WriteElementString("Text", sop.Text);
            writer.WriteElementString("SitName", sop.Properties.SitName);
            writer.WriteElementString("TouchName", sop.Properties.TouchName);

            uint linknum = 0;
            //if (parent != null)
            //    linknum = prim.LocalID - parent.LocalID;

            writer.WriteElementString("LinkNum", linknum.ToString());
            writer.WriteElementString("ClickAction", ((int)sop.ClickAction).ToString());

            WriteShape(writer, sop, sop.Properties, sop.PrimData, options);
            /*
                writer.WriteStartElement("Shape");

                writer.WriteElementString("PathBegin", Primitive.PackBeginCut(sop.PrimData.PathBegin).ToString());
                writer.WriteElementString("PathCurve", ((byte)sop.PrimData.PathCurve).ToString());
                writer.WriteElementString("PathEnd", Primitive.PackEndCut(sop.PrimData.PathEnd).ToString());
                writer.WriteElementString("PathRadiusOffset", Primitive.PackPathTwist(sop.PrimData.PathRadiusOffset).ToString());
                writer.WriteElementString("PathRevolutions", Primitive.PackPathRevolutions(sop.PrimData.PathRevolutions).ToString());
                writer.WriteElementString("PathScaleX", Primitive.PackPathScale(sop.PrimData.PathScaleX).ToString());
                writer.WriteElementString("PathScaleY", Primitive.PackPathScale(sop.PrimData.PathScaleY).ToString());
                writer.WriteElementString("PathShearX", ((byte)Primitive.PackPathShear(sop.PrimData.PathShearX)).ToString());
                writer.WriteElementString("PathShearY", ((byte)Primitive.PackPathShear(sop.PrimData.PathShearY)).ToString());
                writer.WriteElementString("PathSkew", Primitive.PackPathTwist(sop.PrimData.PathSkew).ToString());
                writer.WriteElementString("PathTaperX", Primitive.PackPathTaper(sop.PrimData.PathTaperX).ToString());
                writer.WriteElementString("PathTaperY", Primitive.PackPathTaper(sop.PrimData.PathTaperY).ToString());
                writer.WriteElementString("PathTwist", Primitive.PackPathTwist(sop.PrimData.PathTwist).ToString());
                writer.WriteElementString("PathTwistBegin", Primitive.PackPathTwist(sop.PrimData.PathTwistBegin).ToString());
                writer.WriteElementString("PCode", ((byte)sop.PrimData.PCode).ToString());
                writer.WriteElementString("ProfileBegin", Primitive.PackBeginCut(sop.PrimData.ProfileBegin).ToString());
                writer.WriteElementString("ProfileEnd", Primitive.PackEndCut(sop.PrimData.ProfileEnd).ToString());
                writer.WriteElementString("ProfileHollow", Primitive.PackProfileHollow(sop.PrimData.ProfileHollow).ToString());
                WriteVector(writer, "Scale", sop.Scale);
                writer.WriteElementString("State", sop.PrimData.State.ToString());

                ProfileShape0 shape = (ProfileShape0)sop.PrimData.ProfileCurve;
                writer.WriteElementString("ProfileShape", shape.ToString());
                writer.WriteElementString("HollowShape", sop.PrimData.ProfileHole.ToString());
                writer.WriteElementString("ProfileCurve", sop.PrimData.profileCurve.ToString());

                writer.WriteStartElement("TextureEntry");

                byte[] te;
                if (sop.Textures != null)
                    te = sop.Textures.GetBytes();
                else
                    te = Utils.EmptyBytes;

                writer.WriteBase64(te, 0, te.Length);
                writer.WriteEndElement();

                // FIXME: ExtraParams
                writer.WriteStartElement("ExtraParams"); writer.WriteEndElement();

                writer.WriteEndElement();
                */

            WriteVector(writer, "Scale", sop.Scale);
            writer.WriteElementString("UpdateFlag", "0");
            WriteVector(writer, "SitTargetOrientation", Vector3.UnitZ);
            WriteVector(writer, "SitTargetPosition", Vector3.Zero);
            WriteVector(writer, "SitTargetPositionLL", Vector3.Zero);
            WriteQuaternion(writer, "SitTargetOrientationLL", new Quaternion(0f, 0f, 1f, 0f));
            writer.WriteElementString("ParentID", sop.ParentID.ToString());
            writer.WriteElementString("CreationDate", ((int)Utils.DateTimeToUnixTime(sop.Properties.CreationDate)).ToString());
            writer.WriteElementString("Category", ((int)sop.Properties.Category).ToString());
            writer.WriteElementString("SalePrice", sop.Properties.SalePrice.ToString());
            writer.WriteElementString("ObjectSaleType", ((int)sop.Properties.SaleType).ToString());
            writer.WriteElementString("OwnershipCost", sop.Properties.OwnershipCost.ToString());
            WriteUUID(writer, "GroupID", sop.GroupID);
            WriteUUID(writer, "OwnerID", sop.OwnerID);
            WriteUUID(writer, "LastOwnerID", sop.Properties.LastOwnerID);
            writer.WriteElementString("BaseMask", ((uint)PermissionMask.All).ToString());
            writer.WriteElementString("OwnerMask", ((uint)PermissionMask.All).ToString());
            writer.WriteElementString("GroupMask", ((uint)PermissionMask.All).ToString());
            writer.WriteElementString("EveryoneMask", ((uint)PermissionMask.All).ToString());
            writer.WriteElementString("NextOwnerMask", ((uint)PermissionMask.All).ToString());
            writer.WriteElementString("Flags", sop.Flags.ToString());
            WriteUUID(writer, "SitTargetAvatar", UUID.Zero);

            writer.WriteEndElement();
        }
        public static void WriteShape(XmlTextWriter writer, Primitive prim, Primitive.ObjectProperties props, Primitive.ConstructionData shp, ImportSettings options)
        {
            if (true /*shp != null*/)
            {
                writer.WriteStartElement("Shape");

                writer.WriteElementString("ProfileCurve", shp.ProfileCurve.ToString());

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

                writer.WriteElementString("PathBegin", shp.PathBegin.ToString());
                writer.WriteElementString("PathCurve", shp.PathCurve.ToString());
                writer.WriteElementString("PathEnd", shp.PathEnd.ToString());
                writer.WriteElementString("PathRadiusOffset", shp.PathRadiusOffset.ToString());
                writer.WriteElementString("PathRevolutions", shp.PathRevolutions.ToString());
                writer.WriteElementString("PathScaleX", shp.PathScaleX.ToString());
                writer.WriteElementString("PathScaleY", shp.PathScaleY.ToString());
                writer.WriteElementString("PathShearX", shp.PathShearX.ToString());
                writer.WriteElementString("PathShearY", shp.PathShearY.ToString());
                writer.WriteElementString("PathSkew", shp.PathSkew.ToString());
                writer.WriteElementString("PathTaperX", shp.PathTaperX.ToString());
                writer.WriteElementString("PathTaperY", shp.PathTaperY.ToString());
                writer.WriteElementString("PathTwist", shp.PathTwist.ToString());
                writer.WriteElementString("PathTwistBegin", shp.PathTwistBegin.ToString());
                writer.WriteElementString("PCode", shp.PCode.ToString());
                writer.WriteElementString("ProfileBegin", shp.ProfileBegin.ToString());
                writer.WriteElementString("ProfileEnd", shp.ProfileEnd.ToString());
                writer.WriteElementString("ProfileHollow", shp.ProfileHollow.ToString());
                writer.WriteElementString("State", shp.State.ToString());

                WriteFlags(writer, "ProfileShape", shp.ProfileCurve.ToString(), options);
                WriteFlags(writer, "HollowShape", shp.ProfileHollow.ToString(), options);

                var Sculpt = prim.Sculpt;
                if (Sculpt != null)
                {
                    WriteUUID(writer, "SculptTexture", Sculpt.SculptTexture, options);
                    writer.WriteElementString("SculptType", Sculpt.Type.ToString());
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
                    writer.WriteElementString("FlexiSoftness", Flexi.Softness.ToString());
                    writer.WriteElementString("FlexiTension", Flexi.Tension.ToString());
                    writer.WriteElementString("FlexiDrag", Flexi.Drag.ToString());
                    writer.WriteElementString("FlexiGravity", Flexi.Gravity.ToString());
                    writer.WriteElementString("FlexiWind", Flexi.Wind.ToString());
                    writer.WriteElementString("FlexiForceX", Flexi.Force.X.ToString());
                    writer.WriteElementString("FlexiForceY", Flexi.Force.Y.ToString());
                    writer.WriteElementString("FlexiForceZ", Flexi.Force.Z.ToString());
                }

                Primitive.LightData Light = prim.Light;
                if (Light != null)
                {
                    writer.WriteElementString("LightColorR", Light.Color.R.ToString());
                    writer.WriteElementString("LightColorG", Light.Color.G.ToString());
                    writer.WriteElementString("LightColorB", Light.Color.B.ToString());
                    writer.WriteElementString("LightColorA", Light.Color.A.ToString());
                    writer.WriteElementString("LightRadius", Light.Radius.ToString());
                    writer.WriteElementString("LightCutoff", Light.Cutoff.ToString());
                    writer.WriteElementString("LightFalloff", Light.Falloff.ToString());
                    writer.WriteElementString("LightIntensity", Light.Intensity.ToString());
                }

                writer.WriteElementString("FlexiEntry", (Flexi != null).ToString().ToLower());
                writer.WriteElementString("LightEntry", (Light != null).ToString().ToLower());
                writer.WriteElementString("SculptEntry", (Sculpt != null).ToString().ToLower());

                //todo if (shp.Media != null) writer.WriteElementString("Media", shp.Media.ToXml());

                writer.WriteEndElement(); // Shape
            }
        }
    }
}
