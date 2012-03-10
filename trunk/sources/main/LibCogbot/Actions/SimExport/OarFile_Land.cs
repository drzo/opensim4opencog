using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using OpenMetaverse.Assets;

namespace cogbot.Actions.SimExport
{

    public partial class OarFile
    {
        public static string Serialize(Simulator settings, RegionSettings reg)
        {
            StringWriter sw = new StringWriter();
            XmlTextWriter xtw = new XmlTextWriter(sw);
            xtw.Formatting = Formatting.Indented;
            xtw.WriteStartDocument();

            xtw.WriteStartElement("RegionSettings");

            xtw.WriteStartElement("General");
            xtw.WriteElementString("AllowDamage", ((settings.Flags & RegionFlags.AllowDamage) != 0).ToString());
            xtw.WriteElementString("AllowLandResell", ((settings.Flags & RegionFlags.BlockLandResell) == 0).ToString());
            xtw.WriteElementString("AllowLandJoinDivide", ((settings.Flags & RegionFlags.AllowParcelChanges) != 0).ToString());
            xtw.WriteElementString("BlockFly", ((settings.Flags & RegionFlags.NoFly) != 0).ToString());
            xtw.WriteElementString("BlockLandShowInSearch", ((settings.Flags & RegionFlags.BlockParcelSearch) != 0).ToString());
            xtw.WriteElementString("BlockTerraform", ((settings.Flags & RegionFlags.BlockTerraform) != 0).ToString());
            xtw.WriteElementString("DisableCollisions", ((settings.Flags & RegionFlags.SkipCollisions) != 0).ToString());
            xtw.WriteElementString("DisablePhysics", ((settings.Flags & RegionFlags.SkipPhysics) != 0).ToString());
            xtw.WriteElementString("DisableScripts", ((settings.Flags & RegionFlags.SkipScripts) != 0).ToString());
            xtw.WriteElementString("MaturityRating", settings.Access.ToString());
            xtw.WriteElementString("RestrictPushing", ((settings.Flags & RegionFlags.RestrictPushObject) != 0).ToString());
            xtw.WriteElementString("AgentLimit", reg.AgentLimit.ToString());
            xtw.WriteElementString("ObjectBonus", reg.ObjectBonus.ToString());
            xtw.WriteEndElement();

            xtw.WriteStartElement("GroundTextures");
            xtw.WriteElementString("Texture1", settings.TerrainDetail0.ToString());
            xtw.WriteElementString("Texture2", settings.TerrainDetail1.ToString());
            xtw.WriteElementString("Texture3", settings.TerrainDetail2.ToString());
            xtw.WriteElementString("Texture4", settings.TerrainDetail3.ToString());
            xtw.WriteElementString("ElevationLowSW", settings.TerrainStartHeight00.ToString());
            xtw.WriteElementString("ElevationLowNW", settings.TerrainStartHeight01.ToString());
            xtw.WriteElementString("ElevationLowSE", settings.TerrainStartHeight10.ToString());
            xtw.WriteElementString("ElevationLowNE", settings.TerrainStartHeight11.ToString());
            xtw.WriteElementString("ElevationHighSW", settings.TerrainHeightRange00.ToString());
            xtw.WriteElementString("ElevationHighNW", settings.TerrainHeightRange01.ToString());
            xtw.WriteElementString("ElevationHighSE", settings.TerrainHeightRange10.ToString());
            xtw.WriteElementString("ElevationHighNE", settings.TerrainHeightRange11.ToString());
            xtw.WriteEndElement();

            xtw.WriteStartElement("Terrain");
            xtw.WriteElementString("WaterHeight", settings.WaterHeight.ToString());
            xtw.WriteElementString("TerrainRaiseLimit", reg.TerrainRaiseLimit.ToString());
            xtw.WriteElementString("TerrainLowerLimit", reg.TerrainLowerLimit.ToString());
            xtw.WriteElementString("UseEstateSun", reg.UseEstateSun.ToString());
            xtw.WriteElementString("FixedSun", reg.FixedSun.ToString());
            //todo xtw.WriteElementString("SunPosition", settings.SunPosition.ToString());
            // Note: 'SunVector' isn't saved because this value is owned by the Sun Module, which
            // calculates it automatically according to the date and other factors.
            xtw.WriteEndElement();

            xtw.WriteEndElement();

            xtw.Close();
            sw.Close();

            return sw.ToString();
        }
        public static string Serialize(Parcel landData, ParcelInfo info)
        {
            StringWriter sw = new StringWriter();
            XmlTextWriter xtw = new XmlTextWriter(sw);
            xtw.Formatting = Formatting.Indented;

            xtw.WriteStartDocument();
            xtw.WriteStartElement("LandData");

            xtw.WriteElementString("Area", Convert.ToString(landData.Area));
            xtw.WriteElementString("AuctionID", Convert.ToString(landData.AuctionID));
            xtw.WriteElementString("AuthBuyerID", landData.AuthBuyerID.ToString());
            xtw.WriteElementString("Category", Convert.ToString((sbyte)landData.Category));
            xtw.WriteElementString("ClaimDate", Convert.ToString(landData.ClaimDate));
            xtw.WriteElementString("ClaimPrice", Convert.ToString(landData.ClaimPrice));
            xtw.WriteElementString("GlobalID", info.ID.ToString());
            xtw.WriteElementString("GroupID", landData.GroupID.ToString());
            xtw.WriteElementString("IsGroupOwned", Convert.ToString(landData.IsGroupOwned));
            xtw.WriteElementString("Bitmap", Convert.ToBase64String(landData.Bitmap));
            xtw.WriteElementString("Description", landData.Desc);
            xtw.WriteElementString("Flags", Convert.ToString((uint)landData.Flags));
            xtw.WriteElementString("LandingType", Convert.ToString((byte)landData.Landing));
            xtw.WriteElementString("Name", landData.Name);
            xtw.WriteElementString("Status", Convert.ToString((sbyte)landData.Status));
            xtw.WriteElementString("LocalID", landData.LocalID.ToString());
            var Media = landData.Media;
            xtw.WriteElementString("MediaAutoScale", Convert.ToString(Media.MediaAutoScale));
            xtw.WriteElementString("MediaID", Media.MediaID.ToString());
            xtw.WriteElementString("MediaURL", Media.MediaURL);
            xtw.WriteElementString("MusicURL", landData.MusicURL);
            xtw.WriteElementString("OwnerID", landData.OwnerID.ToString());

            xtw.WriteStartElement("ParcelAccessList");
            foreach (var pal in landData.AccessWhiteList)
            {
                xtw.WriteStartElement("ParcelAccessEntry");
                xtw.WriteElementString("AgentID", pal.AgentID.ToString());
                xtw.WriteElementString("Time", pal.Time.ToString());
                xtw.WriteElementString("AccessList", Convert.ToString((uint)pal.Flags));
                xtw.WriteEndElement();
            }
            foreach (var pal in landData.AccessBlackList)
            {
                xtw.WriteStartElement("ParcelAccessEntry");
                xtw.WriteElementString("AgentID", pal.AgentID.ToString());
                xtw.WriteElementString("Time", pal.Time.ToString());
                xtw.WriteElementString("AccessList", Convert.ToString((uint)pal.Flags));
                xtw.WriteEndElement();
            }
            xtw.WriteEndElement();

            xtw.WriteElementString("PassHours", Convert.ToString(landData.PassHours));
            xtw.WriteElementString("PassPrice", Convert.ToString(landData.PassPrice));
            xtw.WriteElementString("SalePrice", Convert.ToString(landData.SalePrice));
            xtw.WriteElementString("SnapshotID", landData.SnapshotID.ToString());
            xtw.WriteElementString("UserLocation", landData.UserLocation.ToString());
            xtw.WriteElementString("UserLookAt", landData.UserLookAt.ToString());
            xtw.WriteElementString("Dwell", info.Dwell.ToString());
            xtw.WriteElementString("OtherCleanTime", Convert.ToString(landData.OtherCleanTime));

            xtw.WriteEndElement();

            xtw.Close();
            sw.Close();

            return sw.ToString();
        }
    }
}
