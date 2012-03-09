using System;
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

    public class OarFile
    {
        enum ProfileShape : byte
        {
            Circle = 0,
            Square = 1,
            IsometricTriangle = 2,
            EquilateralTriangle = 3,
            RightTriangle = 4,
            HalfCircle = 5
        }

        public static void PackageArchive(string directoryName, string filename)
        {
            const string ARCHIVE_XML = "<?xml version=\"1.0\" encoding=\"utf-16\"?>\n<archive major_version=\"0\" minor_version=\"1\" />";

            TarArchiveWriter archive = new TarArchiveWriter();

            // Create the archive.xml file
            archive.AddFile("archive.xml", ARCHIVE_XML);

            // Add the assets
            string[] files = Directory.GetFiles(directoryName + "/assets");
            foreach (string file in files)
                archive.AddFile("assets/" + Path.GetFileName(file), File.ReadAllBytes(file));

            // Add the objects
            files = Directory.GetFiles(directoryName + "/objects");
            foreach (string file in files)
                archive.AddFile("objects/" + Path.GetFileName(file), File.ReadAllBytes(file));

            // Add the terrain(s)
            files = Directory.GetFiles(directoryName + "/terrains");
            foreach (string file in files)
                archive.AddFile("terrains/" + Path.GetFileName(file), File.ReadAllBytes(file));

            archive.WriteTar(new GZipStream(new FileStream(filename, FileMode.Create), CompressionMode.Compress));
        }

        public static void SavePrims(DoubleDictionary<uint, UUID, Primitive> prims, string path)
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

                    SaveLinkset(linkset, path + "/Primitive_" + linkset.Parent.ID.ToString() + ".xml");
                }
            }
        }

        static void SaveLinkset(Linkset linkset, string filename)
        {
            try
            {
                using (StreamWriter stream = new StreamWriter(filename))
                {
                    XmlTextWriter writer = new XmlTextWriter(stream);
                    SOGToXml2(writer, linkset);
                    writer.Flush();
                }
            }
            catch (Exception ex)
            {
                Logger.Log("Failed saving linkset: " + ex.Message, Helpers.LogLevel.Error);
            }
        }

        static void SOGToXml2(XmlTextWriter writer, Linkset linkset)
        {
            writer.WriteStartElement(String.Empty, "SceneObjectGroup", String.Empty);
            SOPToXml(writer, linkset.Parent, null);
            writer.WriteStartElement(String.Empty, "OtherParts", String.Empty);

            foreach (Primitive child in linkset.Children)
                SOPToXml(writer, child, linkset.Parent);

            writer.WriteEndElement();
            writer.WriteEndElement();
        }

        static void SOPToXml(XmlTextWriter writer, Primitive sop, Primitive parent)
        {
            writer.WriteStartElement("SceneObjectPart");
            writer.WriteAttributeString("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
            writer.WriteAttributeString("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");

            WriteUUID(writer, "CreatorID", sop.Properties.CreatorID);
            WriteUUID(writer, "FolderID", sop.Properties.FolderID);
            writer.WriteElementString("InventorySerial", sop.Properties.InventorySerial.ToString());
            writer.WriteStartElement("TaskInventory"); writer.WriteEndElement();
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

            ProfileShape shape = (ProfileShape)sop.PrimData.ProfileCurve;
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

        static void WriteUUID(XmlTextWriter writer, string name, UUID id)
        {
            writer.WriteStartElement(name);
            writer.WriteElementString("UUID", id.ToString());
            writer.WriteEndElement();
        }

        static void WriteVector(XmlTextWriter writer, string name, Vector3 vec)
        {
            writer.WriteStartElement(name);
            writer.WriteElementString("X", vec.X.ToString());
            writer.WriteElementString("Y", vec.Y.ToString());
            writer.WriteElementString("Z", vec.Z.ToString());
            writer.WriteEndElement();
        }

        static void WriteQuaternion(XmlTextWriter writer, string name, Quaternion quat)
        {
            writer.WriteStartElement(name);
            writer.WriteElementString("X", quat.X.ToString());
            writer.WriteElementString("Y", quat.Y.ToString());
            writer.WriteElementString("Z", quat.Z.ToString());
            writer.WriteElementString("W", quat.W.ToString());
            writer.WriteEndElement();
        }
    }
}
