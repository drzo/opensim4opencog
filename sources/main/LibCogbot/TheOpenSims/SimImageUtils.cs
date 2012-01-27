using System;
using System.Collections.Generic;
using System.Drawing;
using System.Reflection;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public static class SimImageUtils
    {

        public static Dictionary<string, int> ToNamedColors(SimObject av)
        {
            Primitive p = av.Prim;
            if (p==null) return null;
            var t = p.Textures;
            var tf = t.FaceTextures;
            Color4 fnd = Color4.Black;
            bool cf = false;
            foreach (var entryFace in tf)
            {
                if (entryFace == null) continue;
                var fc = entryFace.TextureID;
                var fnd0 = entryFace.RGBA;
                if (fnd0 != Color4.White)
                {
                    fnd = fnd0;
                    cf = true;
                    break;
                }
            }
            if (!cf)
            {
                var dc = t.DefaultTexture;
                if (dc != null)
                {

                    var fnd0 = dc.RGBA;
                    if (fnd0 != Color4.White)
                    {
                        fnd = fnd0;
                    }
                }
            }
            Dictionary<string, int> dict = new Dictionary<string, int>();
            CatchNamedColors(ToColor(fnd), dict, false);
            return dict;
        } 

        public static Dictionary<string, int> ToNamedColors(Color av)
        {
            Dictionary<string, int> dict = new Dictionary<string, int>();
            CatchNamedColors(av, dict, true);
            return dict;
        }
        public static Dictionary<string, int> ToNamedColors(Color4 av)
        {
            return ToNamedColors(ToColor(av));
        }

        private static Color ToColor(Color4 color4)
        {
            return Color.FromArgb((int)(color4.A * 255), (int)(color4.R * 255), (int)(color4.G * 255), (int)(color4.B * 255));
        }

        public static Color[] namedColors = new Color[] { Color.Red, Color.Blue, Color.Yellow, Color.Green, Color.Purple, Color.Orange, Color.White, Color.Black };
        public static void CatchNamedColors(Color av, IDictionary<string, int> dict, bool addZeros)
        {
            int fnd = 1;
            foreach (var c in SortColorMatch(av, AllKnownColors(), 4))
            {
                dict.Add(c.Name.ToLower(), fnd++);
            }
        }

        public static void ColorTest()
        {
            foreach (var test in AllKnownColors())
            {
                Color best = BestColorMatch(test, namedColors);
                Console.WriteLine(" " + test.Name + " is close to " + best);
            }
        }

        static private Color[] _AllKnownColors;
        public static Color[] AllKnownColors()
        {
            if (_AllKnownColors != null) return _AllKnownColors;
            List<Color> testThese = new List<Color>();
            foreach (var f  in typeof(Color).GetProperties(BindingFlags.Public | BindingFlags.Static))
            {
                if (f.PropertyType == typeof(Color))
                {
                    testThese.Add((Color) f.GetGetMethod().Invoke(null, null));
                }
            }
            _AllKnownColors = testThese.ToArray();
            return _AllKnownColors;
        }

        public static Color BestColorMatch(Color test, IEnumerable<Color> namedColors0)
        {
            Color bestMatch = Color.Empty;
            float bestDist = 10000;
            foreach (var color in namedColors0)
            {
                float dist = ColorDist(test, color);
                if (dist < bestDist)
                {
                    bestMatch = color;
                    bestDist = dist;
                }
            }
            return bestMatch;
        }

        public static List<Color> SortColorMatch(Color test, IEnumerable<Color> namedColors0, int keep)
        {
            var v = new List< Color>(namedColors0);
            v.Sort((c1, c2) =>
                       {
                           float distSort = ColorDist(c1, test) - ColorDist(c2, test);
                           return distSort < 0 ? -1 : distSort > 0 ? 1 : 0;
                       });
            if (v.Count > keep)
            {
                v.RemoveRange(keep, v.Count - keep);
            }
            return v;
        }

        public static float ColorDist(Color color1, Color color2)
        {
            Vector3 color1H = GetXY(color1);
            Vector3 color2H = GetXY(color2);
            return Vector3.Distance(color1H, color2H);
        }

        public static Vector3 GetXY(Color color1)
        {
            double fh = color1.GetHue()/57.29577951d;
            float fs = color1.GetSaturation();

            double x = Math.Sin(fh)*fs;
            double y = Math.Cos(fh)*fs;
            return new Vector3((float) x, (float) y, color1.GetBrightness());
        }

        public static Color GetScaledColor(Color average)
        {
            // the Zero number?
            double lowchan = Math.Max(Math.Min(average.R, Math.Min(average.B, average.G)), .00001f);
            // the 255 number?
            double hlowchan = Math.Max(average.R, Math.Max(average.B, average.G));
            double lr = average.R/lowchan;
            double lg = average.G/lowchan;
            double lb = average.B/lowchan;
            double hichan = Math.Max(lr, Math.Max(lb, lg));
            double scaleUp = 255 / hichan;
            double hr = lr * scaleUp;
            double hg = lg * scaleUp;
            double hb = lb * scaleUp;
            return Color.FromArgb((byte)hr, (byte)hg, (byte)hb);
        }

        public static bool ChannelRightDir(int cB, int averageB)
        {
            return cB < 128 ? averageB < 128 : averageB > 128;
        }

        public static float AngDist(float a1, float a2)
        {
            float b1, b2;
            if (a1 > a2)
            {
                b1 = a1;
                b2 = a2;
            }
            else
            {
                b1 = a2;
                b2 = a1;
            }
            float b = b1 - b2;
            if (b < halfRot)
            {
                return b;
            }
            //otherwsie go toward sero
            b1 += (float)halfRot * 2;
            return b1 - b2;
        }

        public const double halfRot = 128;//Math.PI;
    }
}