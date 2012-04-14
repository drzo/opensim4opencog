using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


namespace Aima.Core.Util
{
    using System.Text.RegularExpressions;

    public class Util {
        public static readonly string No = "No";

        public static readonly string Yes = "Yes";

        private static Random r = new Random();

        public static T First<T>(IList<T> l) {
            return l[0];
        }

        public static Regex IdentifierStartRegex = new Regex("[a-zA-Z_$0-9]");
        public static Regex IdentifierPartRegex = new Regex("[a-zA-Z0-9$_+-]");

        public static IList<T> Rest<T>(IList<T> l)
        {
            IList<T> newList = l.ToList();
            newList.RemoveAt(0);
            return newList;
        }

        public static bool RandomBoolean() {
            var trueOrFalse = r.Next(2);
            return (trueOrFalse != 0);
        }

        public static double[] Normalize(double[] probDist) {
            var len = probDist.Length;
            var total = probDist.Aggregate(0.0, (current, d) => current + d);

            var normalized = new double[len];
            if (total != 0) {
                for (var i = 0; i < len; i++) {
                    normalized[i] = probDist[i] / total;
                }
            }
            // TODO: find out if this variable is needed at all
            var totalN = normalized.Aggregate(0.0, (current, d) => current + d);

            return normalized;
        }

        public static IList<double> Normalize(IList<double> values)
        {            
            var valuesAsArray = values.ToArray();
            var normalized = Normalize(valuesAsArray);
            return normalized.ToList();
        }

        public static int Min(int i, int j) {
            return (i > j ? j : i);
        }

        public static int Max(int i, int j) {
            return (i < j ? j : i);
        }

        public static int Max(int i, int j, int k) {
            return Max(Max(i, j), k);
        }

        public static int Min(int i, int j, int k) {
            return Min(Min(i, j), k);
        }

        public static T SelectRandomlyFromList<T>(IList<T> l) 
        {
            return l[r.Next(l.Count())];
        }

        public static T Mode<T>(IList<T> l) {
            var hash = new Dictionary<T, int>();
            foreach (T obj in l) 
            {
                if (hash.ContainsKey(obj)) {
                    hash[obj] = hash[obj] + 1;
                } else {
                    hash[obj] = 1;
                }
            }

            //TODO: can this be converted to LINQ?
            var maxkey = hash.Keys.First();
            foreach (T key in hash.Keys) {
                if (hash[key] > hash[maxkey]) {
                    maxkey = key;
                }
            }
            return maxkey;
        }

        public static string[] YesNo() {
            return new string[] { Yes, No };
        }

        public static double Log2(double d) {
            return System.Math.Log(d) / System.Math.Log(2);
        }

        public static double Information(double[] probabilities) 
        {
            return probabilities.Sum(d => (-1.0 * Log2(d) * d));
        }

        public static List<T> RemoveFrom<T>(IList<T> list, T member) {
            var newList = new List<T>(list);
            newList.Remove(member);
            return newList;
        }

        // TODO: should we just delete this? Doesn't seem to be used and is not supported in C#
        //public static double SumOfSquares<T>(List<T> list) where T: struct //should be number
        //{
        //    double accum = 0;
        //    foreach (T item in list) 
        //    {
        //        accum = accum + (item.doubleValue() * item.doubleValue());
        //    }
        //    return accum;
        //}

        public static string Ntimes(string s, int n) 
        {
            var buf = new StringBuilder();
            for (var i = 0; i < n; i++) 
            {
                buf.Append(s);
            }
            return buf.ToString();
        }

        public static void CheckForNanOrInfinity(double d) 
        {
            if (double.IsNaN(d)) {
                throw new ArgumentException("Not a Number");
            }
            if (double.IsInfinity(d)) {
                throw new ArgumentException("Infinite Number");
            }
        }

        public static int RandomNumberBetween(int i, int j) {
            // i,j bothinclusive 
            return r.Next(j - i + 1) + i;
        }

        public static double CalculateMean(IList<double> lst) 
        {
            var sum = lst.Aggregate(0.0, (current, d) => current + d);
            return sum / lst.Count;
        }

        public static double CalculateStDev(IList<double> values, double mean) {

            var listSize = values.Count;

            var sumOfDiffSquared = values.Select(value => value - mean).Select(diffFromMean => ((diffFromMean * diffFromMean) / (listSize - 1))).Sum();
            var variance = sumOfDiffSquared;
            
            // (listSize - 1);
            // assumes at least 2 members in list.
            return System.Math.Sqrt(variance);
        }

        public static IList<double> NormalizeFromMeanAndStdev(IList<double> values,
                double mean, double stdev)
        {
            return values.Select(d => (d - mean) / stdev).ToList();
        }

        public static double GenerateRandomDoubleBetween(double lowerLimit,
                double upperLimit) 
        {
            return lowerLimit + ((upperLimit - lowerLimit) * r.Next());
        }
    }
}
