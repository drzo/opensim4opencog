using System;
using System.Collections.Generic;
using System.Drawing;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Imaging;
using Radegast.Rendering;

namespace cogbot.TheOpenSims
{
    public class SimTexture : SimAsset
    {
        private Image _ic;

        public Image Image
        {
            get
            {
                if (_ic != null) return _ic;
                ManagedImage m;
                var data = AssetData;
                if (data == null || data.Length == 0) return null;
                if (!OpenMetaverse.Imaging.OpenJPEG.DecodeToImage(AssetData, out m, out _ic)) return null;
                return _ic;
            }
        }
        public Bitmap BitmapImage
        {
            get
            {
                return (Bitmap)Image;
            }
        }

        public Color MeanColor
        {
            get
            {
                Bitmap bm = BitmapImage;
                if (bm == null) return Color.Empty;
                int x = bm.Size.Width;
                int y = bm.Size.Height;
                int x4 = x / 4;
                int xs = x - x4;
                int y4 = y / 4;
                int ys = y - x4;

                long red = 0, blue = 0, green = 0;
                int pc = 0;
                for (int xx = x4; xx < xs; xx++)
                {
                    for (int yy = y4; yy < ys; yy++)
                    {
                        pc++;
                        var pixel = bm.GetPixel(xx, yy);
                        int tp = pixel.R + pixel.G + pixel.B;
                        red += pixel.R;
                        blue += pixel.B;
                        green += pixel.G;
                    }
                }
                area = x * y / 4;
                if (area != pc)
                {
                }
                red = red / pc;
                blue = blue / pc;
                green = green / pc;
                return Color.FromArgb((int)red, (int)green, (int)blue);
            }
        }
        private int area;

        public Dictionary<string, int> ImageStats
        {
            get
            {
                Bitmap bm = BitmapImage;
                if (bm == null) return null;
                Color average = MeanColor;
                Color scaleC = SimImageUtils.GetScaledColor(average);
                Color scaleC2 = SimImageUtils.GetScaledColor(scaleC);
                var dict = new Dictionary<string, int>()
                           {
                               {"area", area},
                               {"m_blue", average.B},
                               {"m_red", average.R},
                               {"m_green", average.G},
                                                      
                               {"a_blue", scaleC2.B},
                               {"a_red", scaleC2.R},
                               {"a_green", scaleC2.G},
                           };

                SimImageUtils.CatchNamedColors(average, dict);
                return dict;
            }
        }


        protected override List<SimAsset> GetParts()
        {
            try
            {
                GuessAssetName();
                Decode(ServerAsset);
            }
            catch (System.Exception ex)
            {
                WriteLine("" + ex);
                //_TypeData = null;
            }
            return new List<SimAsset>() { this };
        }

        public SimTexture(UUID uuid, string name, AssetType type)
            : base(uuid, name, type)
        {
        }

        protected override string GuessAssetName()
        {
            return UnknownName;
        }

        public override float Length
        {
            get { return 2; }
        }

        public override bool IsContinuousEffect
        {
            get { return true; }
        }                
    }
}