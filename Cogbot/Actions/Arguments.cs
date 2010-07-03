using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text.RegularExpressions;
using System;

namespace CommandLine.Utility
{
    /// <summary>
    /// Arguments class
    /// </summary>
    public class Arguments
    {
        // Variables
        // private readonly Dictionary<String,String> Parameters = new Dictionary<string, string>();
        private string[] Original;
        private string[] Keys;
        private string[] Values;
        private bool[] Flags;
        private int[] OriginalPart;
        private int Count = 0;
        // Constructor
        public Arguments(string[] Args)
        {
            Keys = new string[Args.Length];
            Values = new string[Args.Length];
            Flags = new bool[Args.Length];
            Original = Args;
            //Parameters = new StringDictionary();
            Regex Splitter = new Regex(@"^-{1,2}|=",
                RegexOptions.IgnoreCase | RegexOptions.Compiled);

            Regex Remover = new Regex(@"^['""]?(.*?)['""]?$",
                RegexOptions.IgnoreCase | RegexOptions.Compiled);

            string Parameter = null;
            string[] Parts;

            // Valid parameters forms:
            // {-,/,--}param{ ,=,:}((",')value(",'))
            // Examples: 
            // -param1 value1 --param2
            //   /param4=happy -param5 '--=nice=--'
            foreach (string Txt in Args)
            {
                // Look for new parameters (-,/ or --) and a
                // possible enclosed value (=,:)
                Parts = Splitter.Split(Txt, 3);

                switch (Parts.Length)
                {
                    // Found a value (for the last parameter 
                    // found (space separator))
                    case 1:
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                            {
                                Parts[0] =
                                    Remover.Replace(Parts[0], "$1");

                                Add(Parameter, Parts[0]);
                            }
                            Parameter = null;
                        }
                        // else Error: no parameter waiting for a value (skipped)
                        break;

                    // Found just a parameter
                    case 2:
                        // The last parameter is still waiting. 
                        // With no value, set it to true.
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                                AddTrue(Parameter);
                        }
                        Parameter = Parts[1];
                        break;

                    // Parameter with enclosed value
                    case 3:
                        // The last parameter is still waiting. 
                        // With no value, set it to true.
                        if (Parameter != null)
                        {
                            if (!ContainsKey(Parameter))
                                AddTrue(Parameter);
                        }

                        Parameter = Parts[1];

                        // Remove possible enclosing characters (",')
                        if (!ContainsKey(Parameter))
                        {
                            Parts[2] = Remover.Replace(Parts[2], "$1");
                            Add(Parameter, Parts[2]);
                        }

                        Parameter = null;
                        break;
                }
            }
            // In case a parameter is still waiting
            if (Parameter != null)
            {
                if (!ContainsKey(Parameter))
                    AddTrue(Parameter);
            }
        }

        private bool ContainsKey(string k)
        {
            return IndexOf(k) != -1;
        }

        private void AddTrue(string ks)
        {
            string v = ks;
            string k = ToKey(ks);
            int i = IndexOf(k);
            if (i == -1)
            {
                Keys[Count] = ToKey(k);
                Values[Count] = v;
                Flags[Count] = true;
                Count++;
            }
            else
            {
                bool before = IsTrueString(Values[Count]);
                bool after = IsTrueString(ks);

                if (!before)
                {
                    Values[Count] = v;
                    Flags[Count] = true;
                }
            }
        }

        private bool IsTrueString(string v)
        {
            if (String.IsNullOrEmpty(v)) return false;
            if (v.StartsWith("+")) return true;
            if (IsFalseString(v)) return false;
            return true;
        }
        private bool IsFalseString(string v)
        {
            if (String.IsNullOrEmpty(v)) return false;
            v = v.ToLower();
            if (v == "no" || v == "false" || v == "nil" || v.StartsWith("-")) return true;
            return false;
        }

        private void Add(string k, string v)
        {
            k = ToKey(k);
            int i = IndexOf(k);
            if (i == -1)
            {
                Keys[Count] = ToKey(k);
                Values[Count] = v;
                Count++;
            }
            else
            {
                Values[Count] = v;
            }

        }

        static string ToKey(string k)
        {
            while (k.StartsWith("-")) k = k.Substring(1);
            return k;
        }

        // Retrieve a parameter value if it exists 
        // (overriding C# indexer property)
        public string this[string Param]
        {
            get
            {
                int i = IndexOf(Param);
                if (i == -1) return null;
                return Values[i];
            }
        }

        public bool IsTrue(string key)
        {
            if (ContainsKey(key))
            {
                if (!IsTrueString(this[key])) return false;
                return true;
            }
            return false;
        }

        public bool ContainsFlag(string flag)
        {
            while (flag.StartsWith("-"))
            {
                flag = flag.Substring(1);
            }
            return ContainsKey(flag) || ContainsKey("--" + flag);
        }

        public string[] Without(string swipl)
        {
            int i = IndexOf(swipl);
            if (i == -1) return Original;

            var p = new List<string>();
            for (int j = i + 1; j < Count; j++)
            {
                if (Flags[j])
                {
                    p.Add(Values[j]);
                }
                else
                {
                    p.Add(Keys[i]);
                    p.Add(Values[i]);
                }
            }
            return p.ToArray();
        }

        private int IndexOf(string flag)
        {
            flag = ToKey(flag);
            int found = 0;
            foreach (var parameter in Keys)
            {
                if (parameter == flag) return found;
                found++;
            }
            return -1;

        }

        public bool GetWithout(string key, out string[] args)
        {

            int i = IndexOf(key);
            args = Original;
            if (i == -1) return false;
            var p = new List<string>();
            for (int j = 0; j < i; j++)
            {
                if (Flags[j])
                {
                    p.Add(Values[j]);
                }
                else
                {
                    p.Add(Keys[j]);
                    p.Add(Values[j]);
                }
            }
            for (int j = i + 1; j < Count; j++)
            {
                if (Flags[j])
                {
                    p.Add(Values[j]);
                }
                else
                {
                    p.Add(Keys[j]);
                    p.Add(Values[j]);
                }
            }

            args = p.ToArray();
            return true;

        }

        public bool GetAfter(string key, out string[] args)
        {

            int i = IndexOf(key);
            args = Original;
            if (i == -1) return false;
            var p = new List<string>();
            for (int j = i + 1; j < Count; j++)
            {
                if (Flags[j])
                {
                    p.Add(Values[j]);
                }
                else
                {
                    p.Add(Keys[j]);
                    p.Add(Values[j]);
                }
            }

            args = p.ToArray();
            return true;

        }

        public bool GetBoolean(string key, ref bool value)
        {
            string v = this[key];
            if (v == null) return false;
            if (IsFalseString(v))
            {
                value = false;
                return true;
            }
            if (IsTrueString(v))
            {
                value = true;
                return true;                
            }
            return false;
        }
    }
}
