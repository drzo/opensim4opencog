using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public partial class ImportCommand 
    {

        private void UploadTerrain()
        {
            string fn = ExportCommand.terrainFileName;

            lock (ExportCommand.fileWriterLock)
            {
                if (!File.Exists(fn))
                {
                    // upload patches
                    var fn2 = ExportCommand.terrainDir + "terrain.patches";
                    lock (ExportCommand.fileWriterLock)
                    {
                        if (File.Exists(fn2))
                        {
                            TerrainPatch[] loaded = (TerrainPatch[])ExportCommand.FromFile(fn2, true);
                            // TerrainCompressor.CreateLayerDataPacket(loaded, TerrainPatch.LayerType.Land);
                            float[,] hm = GetHeightMap(loaded);
                            hm = SmoothHM(hm);
                            byte[] raw = ToRaw32File(hm);
                            lock (ExportCommand.fileWriterLock) File.WriteAllBytes(fn, raw);
                        }
                    }
                }
            }
            if (File.Exists(fn))
            {
                Client.Estate.UploadTerrain(File.ReadAllBytes(fn), Path.GetFileName(fn));
                Success("Terrain file uploading");
            }
            else
            {
                Failure("unable to find any terrain files");
            }

        }

        private float[,] SmoothHM(float[,] doubles)
        {
            return doubles;
        }

        private byte[] ToRaw32File(float[,] doubles)
        {
            int o = 0;
            byte[] b = new byte[256 * 256];
            for (int y = 0; y < 256; y++)
            {
                for (int x = 0; x < 256; x++)
                {
                    b[o++] = (byte)doubles[x, y];
                }
            }
            return b;
        }

        static float[,] GetHeightMap(TerrainPatch[] Terrain)
        {
            float average = 23;
            float[,] height = new float[256, 256];
            for (int x = 0; x < 256; x++)
            {
                for (int y = 0; y < 256; y++)
                {
                    int patchX = x / 16;
                    int patchY = y / 16;

                    TerrainPatch patch = Terrain[patchY * 16 + patchX];
                    if (patch != null)
                    {
                        height[x, y] = average = patch.Data[(y % 16) * 16 + (x % 16)];
                    }
                    else
                    {
                        height[x, y] = average;
                    }
                }
            }
            return height;
        }
    }
}
