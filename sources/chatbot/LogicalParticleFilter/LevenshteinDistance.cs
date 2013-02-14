using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LogicalParticleFilter1
{
    public static class LevenshteinDistance
    {
        /// <summary>
        /// Compute the distance between two strings.
        /// </summary>
        public static int Compute(string s, string t)
        {
            int n = s.Length;
            int m = t.Length;
            int[,] d = new int[n + 1, m + 1];

            // Step 1
            if (n == 0)
            {
                return m;
            }

            if (m == 0)
            {
                return n;
            }

            // Step 2
            for (int i = 0; i <= n; d[i, 0] = i++)
            {
            }

            for (int j = 0; j <= m; d[0, j] = j++)
            {
            }

            // Step 3
            for (int i = 1; i <= n; i++)
            {
                //Step 4
                for (int j = 1; j <= m; j++)
                {
                    // Step 5
                    int cost = (t[j - 1] == s[i - 1]) ? 0 : 1;

                    // Step 6
                    d[i, j] = Math.Min(
                        Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                        d[i - 1, j - 1] + cost);
                }
            }
            // Step 7
            return d[n, m];
        }

        //http://siderite.blogspot.com/2007/01/super-fast-string-distance-algorithm.html
        // similar to Levenshtein but does a LCS (longest commmon substring) first
        // and approximates from there
        public static float Sift3Distance(string s1, string s2, int maxOffset)
        {
            if (String.IsNullOrEmpty(s1)) return String.IsNullOrEmpty(s2) ? 0 : s2.Length;
            if (String.IsNullOrEmpty(s2)) return s1.Length;
            int c = 0;
            int offset1 = 0;
            int offset2 = 0;
            int lcs = 0;
            while ((c + offset1 < s1.Length) && (c + offset2 < s2.Length))
            {
                if (s1[c + offset1] == s2[c + offset2])
                    lcs++;
                else
                {
                    offset1 = 0;
                    offset2 = 0;
                    for (int i = 0; i < maxOffset; i++)
                    {
                        if ((c + i < s1.Length) && (s1[c + i] == s2[c]))
                        {
                            offset1 = i;
                            break;
                        }
                        if ((c + i < s2.Length) && (s1[c] == s2[c + i]))
                        {
                            offset2 = i;
                            break;
                        }
                    }
                }
                c++;
            }
            return (s1.Length + s2.Length) / 2 - lcs;
        }
        /// <SUMMARY>Computes the Levenshtein Edit Distance between two enumerables.</SUMMARY>
        /// <TYPEPARAM name="T">The type of the items in the enumerables.</TYPEPARAM>
        /// <PARAM name="x">The first enumerable.</PARAM>
        /// <PARAM name="y">The second enumerable.</PARAM>
        /// <RETURNS>The edit distance.</RETURNS>
        //http://blogs.msdn.com/b/toub/archive/2006/05/05/590814.aspx
        public static int EditDistance<T>(IEnumerable<T> x, IEnumerable<T> y)
            where T : IEquatable<T>
        {
            // Validate parameters
            if (x == null) throw new ArgumentNullException("x");
            if (y == null) throw new ArgumentNullException("y");

            // Convert the parameters into IList instances
            // in order to obtain indexing capabilities
            IList<T> first = x as IList<T> ?? new List<T>(x);
            IList<T> second = y as IList<T> ?? new List<T>(y);

            // Get the length of both.  If either is 0, return
            // the length of the other, since that number of insertions
            // would be required.
            int n = first.Count, m = second.Count;
            if (n == 0) return m;
            if (m == 0) return n;

            // Rather than maintain an entire matrix (which would require O(n*m) space),
            // just store the current row and the next row, each of which has a length m+1,
            // so just O(m) space. Initialize the current row.
            int curRow = 0, nextRow = 1;
            int[][] rows = new int[][] { new int[m + 1], new int[m + 1] };
            for (int j = 0; j <= m; ++j) rows[curRow][j] = j;

            // For each virtual row (since we only have physical storage for two)
            for (int i = 1; i <= n; ++i)
            {
                // Fill in the values in the row
                rows[nextRow][0] = i;
                for (int j = 1; j <= m; ++j)
                {
                    int dist1 = rows[curRow][j] + 1;
                    int dist2 = rows[nextRow][j - 1] + 1;
                    int dist3 = rows[curRow][j - 1] +
                        (first[i - 1].Equals(second[j - 1]) ? 0 : 1);

                    rows[nextRow][j] = Math.Min(dist1, Math.Min(dist2, dist3));
                }

                // Swap the current and next rows
                if (curRow == 0)
                {
                    curRow = 1;
                    nextRow = 0;
                }
                else
                {
                    curRow = 0;
                    nextRow = 1;
                }
            }

            // Return the computed edit distance
            return rows[curRow][m];
        }
    }


    public static class Moments {

        //see: http://en.wikipedia.org/wiki/Image_moment

        public static double getBasicMoment(int p, int q, double[,] image) {
                double m = 0;
           
                for (int i = 0, k = image.GetLength (0); i < k; i++)
                {
                    for (int j = 0, l = image.GetLength (1); j < l; j++)
                    {
                        m += Math.Pow(i, p) * Math.Pow(j, q) * image[i,j];
                    }
                }
                return m;
        }


        public static double getCentralMoment(int p, int q, double[,] image)
        {
                double mc = 0;
                double m00 = Moments.getBasicMoment(0, 0, image);
                double m10 = Moments.getBasicMoment(1, 0, image);
                double m01 = Moments.getBasicMoment(0, 1, image);
                double x0 = m10 / m00;
                double y0 = m01 / m00;
                for (int i = 0, k = image.GetLength(0); i < k; i++)
                {
                    for (int j = 0, l = image.GetLength(1); j < l; j++)
                    {
                        mc += Math.Pow((i - x0), p) * Math.Pow((j - y0), q) * image[i,j];
                        }
                }
                return mc;
        }

        public static double getCovarianceXY(int p, int q, double[,] image)
        {
            double mc00 = Moments.getCentralMoment(0, 0, image);
            double mc11 = Moments.getCentralMoment(1, 1, image);
                return mc11 / mc00;
        }


        public static double getVarianceX(int p, int q, double[,] image)
        {
            double mc00 = Moments.getCentralMoment(0, 0, image);
            double mc20 = Moments.getCentralMoment(2, 0, image);
                return mc20 / mc00;
        }


        public static double getVarianceY(int p, int q, double[,] image)
        {
            double mc00 = Moments.getCentralMoment(0, 0, image);
            double mc02 = Moments.getCentralMoment(0, 2, image);
                return mc02 / mc00;
        }


        public static double getNormalizedCentralMoment(int p, int q, double[,] image)
        {
                double gama = ((p + q) / 2) + 1;
                double mpq = Moments.getCentralMoment(p, q, image);
                double m00gama = Math.Pow(Moments.getCentralMoment(0, 0, image), gama);
                return mpq / m00gama;
        }
        

        public static double getHuMoment(double[,] image, int n)
        {
                double result = 0.0;
                
                double
                n20 = Moments.getNormalizedCentralMoment(2, 0, image),
                n02 = Moments.getNormalizedCentralMoment(0, 2, image),
                n30 = Moments.getNormalizedCentralMoment(3, 0, image),
                n12 = Moments.getNormalizedCentralMoment(1, 2, image),
                n21 = Moments.getNormalizedCentralMoment(2, 1, image),
                n03 = Moments.getNormalizedCentralMoment(0, 3, image),
                n11 = Moments.getNormalizedCentralMoment(1, 1, image);
                
                switch (n) {
                case 1:
                        result = n20 + n02;
                        break;
                case 2:
                        result = Math.Pow((n20 - 02), 2) + Math.Pow(2 * n11, 2);
                        break;
                case 3:
                        result = Math.Pow(n30 - (3 * (n12)), 2)
                                        + Math.Pow((3 * n21 - n03), 2);
                        break;
                case 4:
                        result = Math.Pow((n30 + n12), 2) + Math.Pow((n12 + n03), 2);
                        break;
                case 5:
                        result = (n30 - 3 * n12) * (n30 + n12)
                                        * (Math.Pow((n30 + n12), 2) - 3 * Math.Pow((n21 + n03), 2))
                                        + (3 * n21 - n03) * (n21 + n03)
                                        * (3 * Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2));
                        break;
                case 6:
                        result = (n20 - n02)
                                        * (Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2))
                                        + 4 * n11 * (n30 + n12) * (n21 + n03);
                        break;
                case 7:
                        result = (3 * n21 - n03) * (n30 + n12)
                                        * (Math.Pow((n30 + n12), 2) - 3 * Math.Pow((n21 + n03), 2))
                                        + (n30 - 3 * n12) * (n21 + n03)
                                        * (3 * Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2));
                        break;

                default:
                        result=0;
                        break;
                }
                return result;
        }

        public static double[] getHuMoments(double[,] image)
        {
            double result = 0.0;
            double [] results = new double[8];

            double
            n20 = Moments.getNormalizedCentralMoment(2, 0, image),
            n02 = Moments.getNormalizedCentralMoment(0, 2, image),
            n30 = Moments.getNormalizedCentralMoment(3, 0, image),
            n12 = Moments.getNormalizedCentralMoment(1, 2, image),
            n21 = Moments.getNormalizedCentralMoment(2, 1, image),
            n03 = Moments.getNormalizedCentralMoment(0, 3, image),
            n11 = Moments.getNormalizedCentralMoment(1, 1, image);
            int n;
            for (n = 1; n < 7; n++)
            {
                switch (n)
                {
                    case 1:
                        result = n20 + n02;
                        break;
                    case 2:
                        result = Math.Pow((n20 - 02), 2) + Math.Pow(2 * n11, 2);
                        break;
                    case 3:
                        result = Math.Pow(n30 - (3 * (n12)), 2)
                                        + Math.Pow((3 * n21 - n03), 2);
                        break;
                    case 4:
                        result = Math.Pow((n30 + n12), 2) + Math.Pow((n12 + n03), 2);
                        break;
                    case 5:
                        result = (n30 - 3 * n12) * (n30 + n12)
                                        * (Math.Pow((n30 + n12), 2) - 3 * Math.Pow((n21 + n03), 2))
                                        + (3 * n21 - n03) * (n21 + n03)
                                        * (3 * Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2));
                        break;
                    case 6:
                        result = (n20 - n02)
                                        * (Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2))
                                        + 4 * n11 * (n30 + n12) * (n21 + n03);
                        break;
                    case 7:
                        result = (3 * n21 - n03) * (n30 + n12)
                                        * (Math.Pow((n30 + n12), 2) - 3 * Math.Pow((n21 + n03), 2))
                                        + (n30 - 3 * n12) * (n21 + n03)
                                        * (3 * Math.Pow((n30 + n12), 2) - Math.Pow((n21 + n03), 2));
                        break;

                    default:
                        result = 0;
                        break;
                }
                results[n] = result;
            }
            return results;
        }
}
}
