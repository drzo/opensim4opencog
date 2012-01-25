using System;
using System.Collections.Generic;
using System.Drawing;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public static class SimImageUtils
    {
        public static Dictionary<string, int> ToNamedColors(Color av)
        {
            Dictionary<string, int> dict = new Dictionary<string, int>();
            CatchNamedColors(av, dict);
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
        public static void CatchNamedColors(Color av, IDictionary<string, int> dict)
        {
            var av2 = GetScaledColor(GetScaledColor(av));
            foreach (Color c in namedColors)
            {
                if (c.ToArgb() != av.ToArgb())
                {
                    if (!ChannelRightDir(c.R, av2.R) || !ChannelRightDir(c.G, av2.G) || !ChannelRightDir(c.B, av2.B))
                    {
                        dict.Add(c.Name.ToLower(), 0);
                        continue;
                    }
                }
                dict.Add(c.Name.ToLower(), 1);
                float hdif = AngDist(c.GetHue(), av.GetHue());
                float sdif = AngDist(c.GetSaturation(), av.GetSaturation());
                const double pirad256 = 255 / (2 * halfRot);
                dict.Add(c.Name.ToLower() + "_h", (int)(hdif * pirad256));
                dict.Add(c.Name.ToLower() + "_s", (int)(sdif * pirad256));
            }
        }

        public static Color GetScaledColor(Color average)
        {
            double lowchan = Math.Max(Math.Min(average.R, Math.Min(average.B, average.G)), 1d);
            //double hlowchan = Math.Max(average.R, Math.Max(average.B, average.G));
            double lr = average.R/lowchan -0.9;
            double lg = average.G/lowchan -0.9;
            double lb = average.B/lowchan -0.9;
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

        public const double halfRot = 180;//Math.PI;
    }
}