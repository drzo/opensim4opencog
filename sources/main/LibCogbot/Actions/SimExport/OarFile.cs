using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using NotImplementedException=sun.reflect.generics.reflectiveObjects.NotImplementedException;

namespace cogbot.Actions.SimExport
{
    public partial class OarFile
    {

        public static void PrepareDir(string directoryname)
        {
            try
            {
                if (!Directory.Exists(directoryname)) Directory.CreateDirectory(directoryname);
                if (!Directory.Exists(directoryname + "/assets")) Directory.CreateDirectory(directoryname + "/assets");
                if (!Directory.Exists(directoryname + "/objects")) Directory.CreateDirectory(directoryname + "/objects");
                if (!Directory.Exists(directoryname + "/terrains")) Directory.CreateDirectory(directoryname + "/terrains");
            }
            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error); return; }
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

        static void WriteBytes(XmlTextWriter writer, string name, byte[] data)
        {
            writer.WriteStartElement(name);
            byte[] d;
            if (data != null)
                d = data;
            else
                d = Utils.EmptyBytes;
            writer.WriteBase64(d, 0, d.Length);
            writer.WriteEndElement(); // name

        }

        static void WriteFlags(XmlTextWriter writer, string name, string flagsStr, ImportSettings options)
        {
            // Older versions of serialization can't cope with commas, so we eliminate the commas
            writer.WriteElementString(name, flagsStr.Replace(",", ""));
        }


        static void WriteUUID(XmlTextWriter writer, string name, UUID id, ImportSettings options)
        {
            writer.WriteStartElement(name);
            if (options.ContainsKey("old-guids"))
                writer.WriteElementString("Guid", id.ToString());
            else
                writer.WriteElementString("UUID", id.ToString());
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
