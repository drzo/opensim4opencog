using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using MushDLR223.Virtualization;
using UPath = AltAIMLbot.Unifiable;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;
using IndexTargetList = System.Collections.Generic.ICollection<AltAIMLbot.IndexTarget>;
using IndexTargetListImpl = System.Collections.Generic.HashSet<AltAIMLbot.IndexTarget>;

namespace AltAIMLbot
{
    sealed public class UnifiableSerializationSurrogate : ISerializationSurrogate
    {

        // Method called to serialize a Car object
        public void GetObjectData(Object obj,
           SerializationInfo info, StreamingContext context)
        {
            if (obj == null)
            {
                info.AddValue("str", null);
                return;
            }
            Unifiable c = (Unifiable)obj;
            string cRawToString = c.Raw.ToString();
            if (obj is SpecialStringUnifiable)
            {
                info.AddValue("special", c.SpecialCache);
            }
            info.AddValue("str", cRawToString);
        }

        // Method called to deserialize a Car object
        public Object SetObjectData(Object obj,
           SerializationInfo info, StreamingContext context,
           ISurrogateSelector selector)
        {

            Unifiable c = (Unifiable) obj;
            string infoGetString = info.GetString("str");
            Unifiable u;
            if (Unifiable.TryGetUnifiable(infoGetString, out u))
            {
                if (u.GetType() != obj.GetType())
                {
                    AltBot.writeDebugLine("BAD ERROR in Deserialization " + Unifiable.DescribeUnifiable(u) + "!-" +
                                          Unifiable.DescribeUnifiable(obj));
                }
                return u;
            }
            u = Unifiable.MakeUnifiableFromString(infoGetString, false);
            if (u.GetType() != obj.GetType())
            {
                AltBot.writeDebugLine("BAD ERROR in Deserialization " + Unifiable.DescribeUnifiable(u) + "!-" +
                                      Unifiable.DescribeUnifiable(obj));
            }
            return u; // Formatters ignore this return value?
        }
    }

    sealed class FooVer1ToVer2DeserializationBinder : SerializationBinder
    {
        public override Type BindToType(
           string assemblyName, string typeName)
        {

            Type typeToDeserialize = null;

            // For each assemblyName/typeName that you wish to deserialize
            // to a different type, set typeToCreate to the desired type
            String assemVer1 = "MyAssem, Version=1.0.0.0, " +
               "Culture=neutral, PublicKeyToken=b77a5c561934e089";
            String typeVer1 = "Foo";

            if (assemblyName == assemVer1 && typeName == typeVer1)
            {
                assemblyName = assemblyName.Replace("1.0.0.0", "2.0.0.0");
            }

            // To return the type, do this:
            typeToDeserialize = Type.GetType(String.Format("{0}, {1}",
               typeName, assemblyName));

            return typeToDeserialize;
        }
    }

    [Serializable]
    public abstract class Unifiable : BaseUnifiable, IConvertible, IComparable<Unifiable>, Indexable, IndexTarget, IDeserializationCallback, IComparable<string>
    {
        #region UFlags enum

        [Flags]
        public enum UFlags : uint
        {
            NO_FLAGS = 0,
            IS_TRUE = 1,
            IS_FALSE = 2,
            IS_NULL = 4,
            IS_EMPTY = 8,
            IS_EXACT = 16,

            BINDS_STARS = 32,
            LONG_WILDCARD = 64,
            SHORT_WILDCARD = 128,
            LAZY_XML = 256,
            REG_CLASS = 512,
            ONLY_ONE = 1024,
            ONE_OR_TWO = 2048,
            MORE_THAN_ONE = 4096,
            IS_TAG = 8192,
            IS_PUNCT = 16384,
            APPENDABLE = 32768,
            NO_BINDS_STARS = 65536,
            ZERO_OR_MORE = 131072,
        }

        #endregion

        public const float UNIFY_FALSE = 1;
        public const float UNIFY_TRUE = 0;

        public static readonly char[] BRKCHARS = " \r\n\t".ToCharArray();

        protected static readonly Dictionary<string, IndexTargetList> categoryInfosDictionary =
            new Dictionary<string, IndexTargetList>();

        private static readonly Dictionary<string, Unifiable> internedUnifiables =
            new Dictionary<string, Unifiable>(20000);


        public static bool AutoInternUnifiables = false;

        private static readonly Dictionary<string, Unifiable> specialUnifiables = new Dictionary<string, Unifiable>(20);

        public static readonly Unifiable[] DontStore = new Unifiable[0];
        public static readonly string[] DontStoreString = new string[0];
        public static SpecialStringUnifiable Empty
        {
            get { return EmptyRef; }
        }
        public static readonly SpecialStringUnifiable EmptyRef = CreateSpecial("$EMPTY", "");
        public static readonly Unifiable FAIL_NIL = CreateSpecial("$FAIL_NIL", "NIL");
        public static readonly Unifiable TRUE_T = CreateSpecial("$TRUE_T", "T");
        public static readonly Unifiable INCOMPLETE = CreateSpecial("$INCOMPLETE", null);
        public static readonly int MaxCategories = 10000;
        public static readonly Unifiable MISSING = CreateSpecial("$MISSING", /*"OM"*/null);
        public static readonly Unifiable UNSPECIFIED = STAR;//CreateSpecial("$MISSING", /*"OM"*/null);
        public static readonly bool MustBeFast = true;
        public static readonly bool NOCateIndex = true;
        public static readonly Unifiable NULL = CreateSpecial("$NULL", null);
        public static readonly int PrintCategories = 1000;

        public static readonly bool ReturnNullForUnknownNonUnifiables = true;
        public static readonly bool ReturnNullForUnknownUnifiables;
        public static readonly SpecialStringUnifiable SPACE = CreateSpecial("$SPACE", " ");
        public static readonly Unifiable TagInput = CreateSpecial("TAG-INPUT");
        public static readonly Unifiable TagFlag = CreateSpecial("TAG-FLAG");
        public static readonly Unifiable TagEndText = CreateSpecial("TAG-END");
        public static readonly Unifiable TagStartText = CreateSpecial("TAG-START");
        public static readonly Unifiable TagThat = CreateSpecial("TAG-THAT");
        public static readonly Unifiable TagTopic = CreateSpecial("TAG-TOPIC");
        public static readonly bool UpcaseXMLSource = false;

        public UFlags Flags = UFlags.NO_FLAGS;
        public string KeyCache;
        public string SpecialCache;
        public static Unifiable EnglishNothing = Create("Nothing");

        static Unifiable()
        {
            StaticAIMLUtils.FormatProviderConvertor = FormatProviderConvertor0;
        }

        public bool IsEmpty
        {
            get { return IsEMPTY(this); }
        }

        public static Unifiable STAR
        {
            get { return MakeUnifiableFromString("*"); }
        }

        public abstract object Raw { get; }

        public Unifiable LegacyPath
        {
            get { return this; }
        }

        public abstract bool IsAnyText { get; }

        public bool CanMatchZero
        {
            get { return Raw != null && ToUpper().Length == 0; }
        }

        public virtual bool IsWildCard
        {
            get { return true; }
        }

        public virtual bool IsCatchAll
        {
            get { return AsString() == "*"; }
        }

        public virtual Unifiable[] Possibles
        {
            get
            {
                List<Unifiable> possibles = new List<Unifiable>();
                var ar = ToArray();
                bool expanded = false;
                for (int i = 0; i < ar.Length; i++)
                {
                    Unifiable item = ar[i];
                    if (IsMulti(item))
                    {
                        foreach (var e in item.Possibles)
                        {
                            expanded = true;
                            ar[i] = e;
                            var uni = Join(" ", ar, 0, -1);
                            possibles.Add(uni);
                        }
                        // reset
                        ar[i] = item;
                    }
                }
                if (!expanded) return new [] { this };
                return possibles.ToArray();
            }
        }
       
        public abstract bool IsLazy { get; }
        public abstract bool IsLitteral { get; }
        public abstract bool IsLitteralText { get; }
        public abstract bool IsHighPriority { get; }

        public virtual bool IsExactKey
        {
            get { return ToKey() == AsString(); }
        }

        public bool IsStarContent
        {
            get { return OverlyMatchableString == "*"; }
        }

        public bool IsMixedContent
        {
            get { return ToArray().Length > 1; }
        }

        public virtual IndexTargetList CategoryInfos
        {
            get
            {
                string strU = OverlyMatchableString;
                StringUnifiable su = this as StringUnifiable;
                IndexTargetList categoryInfos1 = su == null ? null : su.valueCache as IndexTargetList;
                if (categoryInfos1 != null) return categoryInfos1;
                lock (categoryInfosDictionary)
                {
                    if (!categoryInfosDictionary.TryGetValue(strU, out categoryInfos1))
                    {
                        return null;
                    }
                }
                return categoryInfos1;
            }
            set
            {
                string strU = OverlyMatchableString;
                lock (categoryInfosDictionary) categoryInfosDictionary[strU] = value;
                if (this is StringUnifiable)
                {
                    StringUnifiable su = ((StringUnifiable) this);
                    if (ReferenceEquals(su.valueCache, null))
                    {
                        su.valueCache = ((object) value ?? SpecialCache);
                    }
                }
            }
        }

        public virtual XmlNode PatternNode
        {
            get { return AsNodeXML as XmlNode; }
            set { throw new NotImplementedException(); }
        }

        public virtual Unifiable FullPath
        {
            get { return this; }
        }

        protected abstract string GenerateSpecialName { get; }

        #region IComparable<Unifiable> Members

        public abstract int CompareTo(Unifiable other);

        #endregion

        #region Indexable Members

        public override sealed string SpecialName
        {
            get
            {
                if (SpecialCache == null)
                {
                    SpecialCache = GenerateSpecialName;
                }
                return SpecialCache;
            }
        }

        public override string OverlyMatchableString
        {
            get
            {
                if (KeyCache == null)
                {
                    string local = StaticAIMLUtils.Trim(SpecialName);
                    KeyCache = StaticAIMLUtils.SPLITMATCHABLE(SpecialName);
                }
                return KeyCache;
            }
        }

        public virtual bool AddCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            throw new NotImplementedException();
        }

        public virtual bool RemoveCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            throw new NotImplementedException();
        }

        #endregion

        public abstract string ToDebugString();

        public static long LowMemExpireUnifiableCaches()
        {
            long total = 0;
            lock (internedUnifiables)
                foreach (Unifiable internedUnifiable in internedUnifiables.Values)
                {
                    Unifiable u = internedUnifiable; //.Value;
                    if (u != null)
                    {
                        total += u.RunLowMemHooks();
                    }
                }
            return total;
        }

        public abstract int RunLowMemHooks();

        public static IConvertible FormatProviderConvertor0(IConvertible arg, Type solid)
        {
            IConvertible output = arg;
            if (ReferenceEquals(arg, null))
            {
                return output;
            }
            if (solid.IsInstanceOfType(arg))
            {
                return arg;
            }
            Type rType = arg.GetType();
            if (solid == typeof (Unifiable) && rType == typeof (string))
            {
                string u = (string) arg;
                return (Unifiable) u;
            }
            if (solid == typeof (string) && rType == typeof (Unifiable))
            {
                Unifiable u = (Unifiable) arg;
                return u.AsString();
            }

            try
            {
                string u = arg.ToString(StaticAIMLUtils.FormatProvider);
                bool wasIncomplete = IsIncomplete(u);
                if (solid == typeof (string))
                {
                    if (wasIncomplete)
                    {
                        if (ReturnNullForUnknownUnifiables) return INCOMPLETE;
                    }
                    return u;
                }
                if (solid == typeof (Double))
                {
                    if (wasIncomplete)
                    {
                        u = "0.0";
                        if (ReturnNullForUnknownNonUnifiables) return INCOMPLETE;
                    }
                    return Double.Parse(u);
                }
                if (solid == typeof (Int32))
                {
                    if (wasIncomplete)
                    {
                        u = "0";
                        if (ReturnNullForUnknownNonUnifiables) return INCOMPLETE;
                    }
                    return Int32.Parse(u);
                }
                if (solid.IsSubclassOf(typeof (Unifiable)) || typeof (Unifiable).IsSubclassOf(solid))
                {
                    if (wasIncomplete)
                    {
                        if (ReturnNullForUnknownUnifiables) return INCOMPLETE;
                    }
                    return Create(u);
                }
            }
            catch (Exception exception)
            {
                writeToLog("ERROR FormatProviderConvertor " + arg + " to " + solid);
            }
            //if (FormatProvider == null) FormatProvider = ;//.;//new IFormatProvider();
            object format = StaticAIMLUtils.FormatProvider.GetFormat(solid);
            object oo = arg.ToType(solid, StaticAIMLUtils.FormatProvider);
            return (IConvertible) oo;
        }

        public bool IsFlag(UFlags f)
        {
            return (f & Flags) != 0;
        }


        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public abstract override bool Equals(object obj);

        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <returns></returns>
        public abstract override int GetHashCode();

        public static implicit operator string(Unifiable value)
        {
            if (ReferenceEquals(value, null))
            {
                return null;
            }
            return value.AsString();
        }

        public static implicit operator Unifiable(string value)
        {
            if (value == null) return null;
            Unifiable unif = MakeUnifiableFromString(value, false);
            if (value.Contains(" "))
            {
                return unif;
            }
            return unif;
        }

        public static implicit operator Unifiable(XmlNode value)
        {
            if (value == null) return null;
            Unifiable unif = CreateFromXml(value);
            return unif;
        }

        /*
        public static implicit operator Unifiable(XmlNodeList childNodes)
        {
            StringAppendableUnifiableImpl stringAppendable = CreateAppendable();
            try
            {
                List<Unifiable> u = new List<Unifiable>();
                CreateUnifableForList(childNodes, stringAppendable, u);
                StringUnifiable unifiable = new StringUnifiable("");
                unifiable.str = stringAppendable;
                unifiable.splittedCache = u.ToArray();
                return unifiable;
            }
            catch (Exception e)
            {
                AltBot.writeDebugLine("" + e.Message + ": " + " " + e.StackTrace + "\n" + stringAppendable);
                throw;
            }
        }
        */

        private static Unifiable MakeUnifiableFromString(string value)
        {
            var su = MakeUnifiableFromString(value, true);
            return su;
        }
        public static void LoadUnifiables(string path, BinaryFormatter bf)
        {
            if (!AutoInternUnifiables) return;
            FileInfo fi = new FileInfo(path);
            if (!fi.Exists)
            {
                return;
            }

            Stream loadFile = HostSystem.OpenRead(path);

            var bb = (int)bf.Deserialize(loadFile);
            while (bb-- > 0)
            {
                Unifiable u = (Unifiable) bf.Deserialize(loadFile);
            }

        }
        public static void SaveUnifiables(string path, BinaryFormatter bf)
        {
            if (!AutoInternUnifiables) return;
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                fi.Delete();
            }

            FileStream saveFile = HostSystem.Create(path);
            

            lock (internedUnifiables) try
            {
                bf.Serialize(saveFile, internedUnifiables.Count);
                foreach (KeyValuePair<string, Unifiable> pair in internedUnifiables)
                {
                    try
                    {
                        //var testO = this.RootNode.AllDecendantTemplates[0];
                        //bf.Serialize(saveFile, testO);
                        bf.Serialize(saveFile, pair.Value);
                        //bf.Serialize(saveFile, this.PostParallelRootNode);

                    }
                    catch(Exception e)
                    {
                        writeToLog("cant serialize: " + DescribeUnifiable(pair.Value));
                    }
                }
            }
            finally
            {
                saveFile.Close();
            }
        }

        public static BinaryFormatter GetBinaryFormatter()
        {
            SurrogateSelector newSurrogateSelector = new SurrogateSelector();
            StreamingContext newStreamingContext = new StreamingContext();
            newSurrogateSelector.AddSurrogate(typeof(BestUnifiable), newStreamingContext, new UnifiableSerializationSurrogate());
            newSurrogateSelector.AddSurrogate(typeof(StringUnifiable), newStreamingContext, new UnifiableSerializationSurrogate());
            newSurrogateSelector.AddSurrogate(typeof(Unifiable), newStreamingContext, new UnifiableSerializationSurrogate());
            return new BinaryFormatter(newSurrogateSelector, newStreamingContext);
        }


        public static bool TryGetUnifiable(string str, out Unifiable u)
        {
            lock (internedUnifiables)
                return (internedUnifiables.TryGetValue(str, out u));
        }
        public static void AddUnifiable(string str, Unifiable u)
        {
            lock (internedUnifiables) internedUnifiables.Add(str, u);
        }

        public static Unifiable MakeUnifiableFromString(string value, bool useTrimmingRules)
        {
            if (value == null) return null;
            if (value == "") return EmptyRef;
            Unifiable u;
            if (true)
                lock (internedUnifiables)
                {
                    if (internedUnifiables.TryGetValue(value, out u))
                    {
                        return u;
                    }
                }
            if (useTrimmingRules && StaticAIMLUtils.ContainsWhitespace(value))
            {
                value = StaticAIMLUtils.CleanWhitepaces(value);
            }
            string key = Intern(value);
            //if (value != key)
            {
                //   writeToLog("Triming? '" + value + "'");
                value = key;
                //return new StringUnifiable(value);
            }
            if (AutoInternUnifiables)
                lock (internedUnifiables)
                {
                    if (!internedUnifiables.TryGetValue(key, out u))
                    {
                        u = MakeCorrectUnifiable(value);
                        internedUnifiables[key] = u;
                        // ReSharper disable ConditionIsAlwaysTrueOrFalse
                        if (false && (internedUnifiables.Count%10000) == 0)
                            // ReSharper restore ConditionIsAlwaysTrueOrFalse
                        {
                            writeToLog("DEBUG9 internedUnifiables.Count=" + internedUnifiables.Count);
                        }
                    }
                    return u;
                }
            u = MakeCorrectUnifiable(value);
            return u;
        }

        private static Unifiable MakeCorrectUnifiable(string value)
        {
            Unifiable u;
            if (value.StartsWith("<xor>") || IsORSyntax(value))
            {
                u = MakeBestUnifiable(value);
            } else
            {
                u  = new StringUnifiable(value, true);                            
            }
            return u;
        }

        public static bool IsORSyntax(string value)
        {
            return value.StartsWith("(") && value.EndsWith(")") && value.Contains("|");
        }

        private static Unifiable MakeBestUnifiable(string value)
        {
            var b = new BestUnifiable(value, false);
            return b;
        }

        public static string Intern(string cleanWhitepaces)
        {
            return string.Intern(cleanWhitepaces);
        }


        public static string Join(string sep, Unifiable[] values, int startIndex, int count)
        {
            if (count == 0 || values.Length == 0)
            {
                return string.Empty;
            }
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, FromArrayOf(values), startIndex, count);
        }

        public static Unifiable Join(string sep, string[] values, int startIndex, int count)
        {
            if (count == 0 || values.Length == 0)
            {
                return Empty;
            }
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, values, startIndex, count);
        }

        public static Unifiable[] arrayOf(ICollection strs)
        {
            if (strs.Count == 0) return DontStore;
            Unifiable[] it = new Unifiable[strs.Count];
            int i = 0;
            foreach (object str in strs)
            {
                it[i] = Create(str);
                i++;
            }
            return it;
        }

        public static Unifiable[] arrayOf(IEnumerable strs)
        {
            List<Unifiable> it = new List<Unifiable>();
            int i = 0;
            foreach (object str in strs)
            {
                it.Add(Create(str));
            }
            if (it.Count == 0) return DontStore;
            return it.ToArray();
        }

        public static string[] FromArrayOf(Unifiable[] tokens)
        {
            string[] it = new string[tokens.Length];
            if (tokens == DontStore) return DontStoreString;
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = tokens[i].AsString();
            }
            return it;
        }

        public static bool operator ==(Unifiable t, string s)
        {
            if (s == null) return IsNull(t);
            return EQ(s, t);
        }

        public static bool operator !=(Unifiable t, string s)
        {
            return !(t == s);
        }

        virtual public bool SameMeaning(Unifiable t)
        {
            if (t is BestUnifiable) return t.SameMeaning(this);
            return SAME_MEANING(this, t);
        }

        public static bool SAME_MEANING(Unifiable t, Unifiable s)
        {
            return SAME_KEY(t, s, false);
        }

        public static bool SAME_KEY(Unifiable t, Unifiable s, bool caseSensitive)
        {
            if (!ReferenceEquals(t, null)) return t.SameMeaningCS(s, caseSensitive);
            return ReferenceEquals(s, null);
        }
        
        public virtual bool SameMeaningCS(Unifiable s, bool caseSensitive)
        {
            if (s is BestUnifiable) return s.SameMeaningCS(this, caseSensitive);
            if (ReferenceEquals(this, s)) return true;
            bool null2 = ReferenceEquals(s, null);
            if (null2) return false;
            if (Equals(SpecialName, s.SpecialName))
            {
                return true;
            }
            if (caseSensitive)
            {
                if (AsString() == s.AsString())
                {
                    return true;
                }
                return false;
            }
            if (ToUpper() == s.ToUpper())
            {
                return true;
            }
            return false;
        }

        public static bool operator ==(Unifiable t, object s)
        {
            if (s == null) return IsNull(t);
            if (s is Unifiable) return SAME_KEY(t, (Unifiable)s, true);
            if (s is string)
            {
                string ss = (string) s;
                if (t == ss)
                {
                    return true;
                }
                return false;
            }
            if (t == null) return IsNull(s);
            return EQ(CreateEQ(s), t);
        }

        public static bool operator !=(Unifiable t, object s)
        {
            return !(t == s);
        }

        private static string CreateEQ(object o)
        {
            if (o == null) return null;
            if (IsNull(o)) return null;
            if (IsEMPTY(o)) return "";
            if (IsIncomplete(o)) return "OM";
            return Create(o);
        }

        public static bool EQ(string t, string u)
        {
            if (t == u) return true;
            bool unull = IsNull(u);
            if (t == null || t == "$NULL")
            {
                return unull;
            }
            int tlen = t.Length;
            if (unull)
            {
                return IsIncomplete(t);
            }
            string uutrim = u.Trim(new[] {' '});
            int uutrimLen = uutrim.Length;
            if (uutrimLen < 2) return t == uutrim;
            if (tlen == 0)
            {
                return (uutrimLen == 0);
                return IsEMPTY(u);
            }

            if (IsIncomplete(t))
            {
                return IsIncomplete(u);
            }
            if (IsMissing(t))
            {
                return IsMissing(u);
            }
            if (IsValue(u))
            {
                return ToUpper(t) == u.ToUpper();
            }
            if (IsNull(u) || IsIncomplete(u) || IsEMPTY(u) || IsMissing(u))
            {
                return false;
            }
            if (ToUpper(t) == u.ToUpper()) return true;
            return false;
        }

        public static Unifiable operator +(string u, Unifiable more)
        {
            if (u.Length == 0)
            {
                return more;
            }
            if (more == null)
            {
                writeToLog("ERROR: appending NULL");
                return u + " -NULL-";
            }
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0)
            {
                return u;
            }
            return MakeUnifiableFromString(u + more.AsString());
        }

        public static Unifiable operator +(Unifiable u, string more)
        {
            if (more == null)
            {
                return u; //.AsString();
            }
            return MakeUnifiableFromString("" + u.AsString() + more);
        }

        public static Unifiable operator +(Unifiable u, Unifiable more)
        {
            if (IsNullOrEmpty(more))
            {
                return u;
            }
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0)
            {
                return u;
            }
            return MakeUnifiableFromString(u.AsString() + " " + moreAsString);
        }

        public static SpecialStringUnifiable CreateSpecial(string key)
        {
            return CreateSpecial(key, key);
        }

        public static SpecialStringUnifiable CreateSpecial(string key, string value)
        {
            Dictionary<string, Unifiable> dict = specialUnifiables;
            lock (dict)
            {
                Unifiable u;
                if (!dict.TryGetValue(key, out u))
                {
                    key = Intern(key);
                    u = dict[key] = new SpecialStringUnifiable(value, key);
                    u.Flags |= UFlags.IS_TAG;
                    if (value != key && value != null)
                    {
                        value = Intern(value);
                        dict[value] = u;
                        internedUnifiables[value] = u;
                    }
                }
                return (SpecialStringUnifiable) u;
            }
        }

        public static Unifiable Create(object p)
        {
            if (p is string)
            {
                return MakeUnifiableFromString((string) p);
            }
            if (p is Unifiable) return (Unifiable) p;
            if (p is XmlNode)
            {
                XmlNode n = (XmlNode) p;
                return CreateFromXml(n);
            }
            // @TODO change this?
            if (ReferenceEquals(p, null))
            {
                return NULL;
            }
            if (true.Equals(p)) return TRUE_T;
            if (false.Equals(p)) return FAIL_NIL;
            throw new NotImplementedException();
            return MakeUnifiableFromString(p.ToString());
        }

        public static Unifiable CreateFromXml(XmlNode n)
        {
            string inner = StaticAIMLUtils.ToXmlValue(n);
            var stringUnifiable = MakeUnifiableFromString(inner);
            stringUnifiable.OfferNode(n, inner);
            return stringUnifiable;
        }

        public abstract void OfferNode(XmlNode node, string inner);

        public int CompareTo(string other)
        {
            if (other == null) return 1;
            return -other.CompareTo(AsString());
        }

        public override string ToString()
        {
            //writeToLog("ToSTring");
            return GetType().Name + "={" + Raw + "}"; //Raw.ToString();}
        }

        internal static StringAppendableUnifiableImpl CreateAppendable()
        {
            return new StringAppendableUnifiableImpl();
            //   return new StringBuilder(10);
        }

        public virtual bool IsFalse()
        {
            return IsNullOrEmpty(this);
        }

        public abstract bool IsTag(string s);

        public static void writeToLog(string message, params object[] args)
        {
            try
            {
                AltBot.writeDebugLine("UNIFYABLETRACE: " + message, args);
            }
            catch
            {
            }
        }

        public abstract bool ConsumePath(int at, string[] tokens,
                                         out string fw, out Unifiable right,
                                         out int newAt, SubQuery query);

        public bool WillUnify(Unifiable other, SubQuery query)
        {
            if (IsAnyText) return !IsNullOrEmpty(other);
            if (CanMatchZero) return other.CanMatchZero;
            return Unify(other, query) == UNIFY_TRUE;
        }

        public bool CanUnify(Unifiable other, SubQuery query)
        {
            if (IsAnyText) return !IsNullOrEmpty(other);
            if (CanMatchZero) return other.CanMatchZero;
            return Unify(other, query) == UNIFY_TRUE;
        }

        public abstract float Unify(Unifiable unifiable, SubQuery query);

        public static bool IsStringMatch(string s, string u)
        {
            if (s == u) return true;
            if (s.ToUpper().Trim() == u.ToUpper().Trim())
            {
                return true;
            }
            if (MustBeFast) return false;
            return IsMatch2(s, u);
        }

        public static bool IsMatch2(string st, string actualValue)
        {
            if (ReferenceEquals(st, actualValue)) return true;
            string that = " " + actualValue + " ";
            string thiz = " " + st + " ";
            if (thiz == that)
            {
                return true;
            }
            if (thiz.ToLower() == that.ToLower())
            {
                return true;
            }
            if (TwoMatch0(that, thiz))
            {
                return true;
            }
            string a1 = st;
            string a2 = actualValue;
            thiz = " " + a1 + " ";
            that = " " + a2 + " ";
            if (TwoMatch0(that, thiz))
            {
                return true;
            }
            if (TwoSemMatch(a1, a2))
            {
                return true;
            }
            if (st.StartsWith("~"))
            {
                string type = st.Substring(1);
                bool b = NatLangDb.IsWordClass(actualValue, type);
                if (b)
                {
                    return true;
                }
                return b;
            }
            return false;
        }


        private static bool TwoSemMatch(string that, string thiz)
        {
            return false;
            if (NatLangDb.IsWordClassCont(that, " determ") && NatLangDb.IsWordClassCont(thiz, " determ"))
            {
                return true;
            }
            return false;
        }

        private static bool TwoMatch0(string s1, string s2)
        {
            if (s1 == s2) return true;
            if (MustBeFast) return false;
            Regex matcher = new Regex(s1.Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"), RegexOptions.IgnoreCase);
            bool b = matcher.IsMatch(s2);
            if (b)
            {
                return true;
            }
            return b;
        }

        public virtual Unifiable ToCaseInsensitive()
        {
            return this;
        }

        public virtual Unifiable Frozen(SubQuery subquery)
        {
            return ToValue(subquery);
        }

        public abstract string ToValue(SubQuery subquery);
        public abstract string AsString();

        public string AUnifyString
        {
            get { return AsString(); }
        }

        public virtual Unifiable ToPropper()
        {
            return this;
        }

        public virtual Unifiable Trim()
        {
            return this;
        }

        public virtual string ToLower()
        {
            return ToLower(AsString());
        }

        //public abstract Unifiable[] Split(Unifiable[] unifiables, StringSplitOptions options);
        //public abstract Unifiable[] Split();


        // join functions
        public abstract void Append(Unifiable part);
        public abstract void Append(string part);
        public abstract void Clear();

        public abstract Unifiable First { get; }

        public abstract Unifiable Rest { get; }

        public abstract object AsNodeXML { get; }

        public static Unifiable MakePath(Unifiable unifiable)
        {
            return unifiable;
        }

        public abstract Unifiable[] ToArray();

        public virtual bool StoreWildCard()
        {
            return true;
        }

        public static string ToStringLValue(object unifiable)
        {
            if (ReferenceEquals(null, unifiable))
            {
                return null;
            }
            if (unifiable is string)
            {
                return ToLower(((string) unifiable));
            }
            if (unifiable is Unifiable)
            {
                return ToStringLValue(((Unifiable) unifiable).Raw);
            }
            return ToStringLValue("" + unifiable);
        }

        public static string ToVMString(object unifiable)
        {
            if (unifiable is Unifiable)
            {
                return ToVMString(((Unifiable) unifiable).Raw);
            }
            if (ReferenceEquals(null, unifiable))
            {
                return "-NULL-";
            }
            return "" + unifiable;
        }

        public abstract string ToUpper();

        public static string GuessWildCardIndex(char c0, char cN, string specialIndex)
        {
            if (c0 == '*' && '*' == cN) return NoteSpecialIndexer(specialIndex, "*");
            if (c0 == '_' && '_' == cN) return NoteSpecialIndexer(specialIndex, "_");

            return null;

            if (c0 == '<' && cN == '>') return NoteSpecialIndexer(specialIndex, "<>");
            if (c0 == '~' || cN == '~') return NoteSpecialIndexer(specialIndex, "<>");
            if (c0 == '#') return NoteSpecialIndexer(specialIndex, "<>");

            if (c0 == '*' || cN == '*') return NoteSpecialIndexer(specialIndex, "*");
            if (c0 == '_' || cN == '_') return NoteSpecialIndexer(specialIndex, "_");

            return null;
        }

        private static string NoteSpecialIndexer(string index, string idx)
        {
            //DLRConsole.DebugWriteLine("NoteSpecialIndexer " + index);
            return idx;
        }

        public abstract double Strictness { get; }

        public string ScriptingName
        {
            get { return ToVMString(this); }
        }

        public decimal Length
        {
            get { return AsString().Length; }
        }

        public static string DescribeUnifiable(Object value)
        {
            if (value == null) return "-UOBJECT-";
            string s = value.GetType().Name;
            if (ReferenceEquals(value, NULL)) s += "-EQ-NULL-";
            if (IsNull(value)) s += "-IsNull-";
            if (IsIncomplete(value)) s += "-IsIncomplete-";
            if (IsMissing(value)) s += "-IsMissing-";
            if (IsEMPTY(value)) s += "-IsEMPTY-";
            if (value is Unifiable)
            {
                s += string.Format("='{0}'", DescribeUnifiable(((Unifiable) value).Raw));
            }
            else if (value is string)
            {
                s += string.Format("=\"{0}\"", value);
            }
            else
            {
                s += string.Format("=[{0}]", value);
            }
            return s;
        }


        internal bool LoopsFrom(string innerXml)
        {
            string p = StaticAIMLUtils.MakeAimlMatchable(FullPath.AsString().Replace("_", "*"));
            p = "<srai>" + p + "</srai>";

            string t = StaticAIMLUtils.MakeAimlMatchable(innerXml);

            if (t.Contains(p))
            {
                return true;
            }
            return false;
        }

        internal bool DivergesFrom(TemplateInfo newTemplateInfo, out Unifiable from, out Unifiable to)
        {
            if (true)
            {
                from = "";
                to = "";
                return false;
            }
            string p = StaticAIMLUtils.MakeAimlMatchable(FullPath.AsString().Replace("_", "*"));
            p = "<srai>" + p + "</srai>";
            string t = StaticAIMLUtils.MakeAimlMatchable(newTemplateInfo.TemplateXml.InnerXml);

            int firstTP = FirstMismatch(t, p);
            int lastTP = LastMismatch(t, p);
            int firstPT = FirstMismatch(p, t);
            int lastPT = LastMismatch(p, t);
            from = "";
            to = "";
            return false;
        }

        private static int FirstMismatch(string s1, string s2)
        {
            int i = 0;
            for (; i < s1.Length; i++)
            {
                if (s1[i] == s2[i]) continue;
                return i - 1;
            }
            return i - 1;
        }

        private static int LastMismatch(string s1, string s2)
        {
            int i = s1.Length - 1;
            for (; i >= 0; i--)
            {
                if (s1[i] == s2[i]) continue;
                return i - 1;
            }
            return i - 1;
        }

        public static bool IsMulti(object value)
        {
            return value is BestUnifiable;
        }

        #region Implementation of IConvertible

        /// <summary>
        /// Returns the <see cref="T:System.TypeCode"/> for this instance.
        /// </summary>
        /// <returns>
        /// The enumerated constant that is the <see cref="T:System.TypeCode"/> of the class or value type that implements this interface.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        TypeCode IConvertible.GetTypeCode()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent Boolean value using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A Boolean value equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        bool IConvertible.ToBoolean(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent Unicode character using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A Unicode character equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        char IConvertible.ToChar(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 8-bit signed integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 8-bit signed integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        sbyte IConvertible.ToSByte(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 8-bit unsigned integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 8-bit unsigned integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        byte IConvertible.ToByte(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 16-bit signed integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 16-bit signed integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        short IConvertible.ToInt16(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 16-bit unsigned integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 16-bit unsigned integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        ushort IConvertible.ToUInt16(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 32-bit signed integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 32-bit signed integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        int IConvertible.ToInt32(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 32-bit unsigned integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 32-bit unsigned integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        uint IConvertible.ToUInt32(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 64-bit signed integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 64-bit signed integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        long IConvertible.ToInt64(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent 64-bit unsigned integer using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An 64-bit unsigned integer equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        ulong IConvertible.ToUInt64(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent single-precision floating-point number using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A single-precision floating-point number equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        float IConvertible.ToSingle(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent double-precision floating-point number using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A double-precision floating-point number equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        double IConvertible.ToDouble(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent <see cref="T:System.Decimal"/> number using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Decimal"/> number equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        decimal IConvertible.ToDecimal(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent <see cref="T:System.DateTime"/> using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.DateTime"/> instance equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        DateTime IConvertible.ToDateTime(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Converts the value of this instance to an equivalent <see cref="T:System.String"/> using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.String"/> instance equivalent to the value of this instance.
        /// </returns>
        /// <param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        string IConvertible.ToString(IFormatProvider provider)
        {
            return AsString();
        }

        /// <summary>
        /// Converts the value of this instance to an <see cref="T:System.Object"/> of the specified <see cref="T:System.Type"/> that has an equivalent value, using the specified culture-specific formatting information.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Object"/> instance of type <paramref name="conversionType"/> whose value is equivalent to the value of this instance.
        /// </returns>
        /// <param name="conversionType">The <see cref="T:System.Type"/> to which the value of this instance is converted. 
        ///                 </param><param name="provider">An <see cref="T:System.IFormatProvider"/> interface implementation that supplies culture-specific formatting information. 
        ///                 </param><filterpriority>2</filterpriority>
        object IConvertible.ToType(Type conversionType, IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        #endregion

        virtual public string ToKey()
        {
            return ToUpper();
        }

        public abstract bool WillMatch(string word, SubQuery query);

        public abstract bool WillMatch0(string word, SubQuery query);



        public abstract void SetFromString(string inlist);

        #region IDeserializationCallback Members

        void IDeserializationCallback.OnDeserialization(object sender)
        {
           // throw new NotImplementedException();
        }

        #endregion

    }

    public class UnifiableExtensionMethods
    {
        public static bool IsEMPTY(Object unifiable)
        {
            return StaticAIMLUtils.IsEMPTY(unifiable);
        }


        public static bool IsMissing(Object unifiable)
        {
            return StaticAIMLUtils.IsMissing(unifiable);
        }

        public static bool IsNull(Object unifiable)
        {
            return StaticAIMLUtils.IsNull(unifiable);
        }
        public static bool IsValue(Unifiable unifiable)
        {
            return StaticAIMLUtils.IsValue(unifiable);
        }
        public static bool IsTrue(Unifiable unifiable)
        {
            return StaticAIMLUtils.IsTrue(unifiable);
        }
        public static bool IsFalse(Unifiable unifiable)
        {
            return StaticAIMLUtils.IsFalse(unifiable);
        }
        public static bool IsNullOrEmpty(Object unifiable)
        {
            return StaticAIMLUtils.IsNullOrEmpty(unifiable);
        }
        public static bool IsIncomplete(Object unifiable)
        {
            return StaticAIMLUtils.IsIncomplete(unifiable);
        }
        public static string ToLower(string unifiable)
        {
            return StaticAIMLUtils.ToLower(unifiable);
        }
        public static string ToUpper(string unifiable)
        {
            return StaticAIMLUtils.ToUpper(unifiable);
        }
        public static string Trim(string unifiable)
        {
            return StaticAIMLUtils.Trim(unifiable);
        }

        public static bool IsUnknown(object unifiable)
        {
            return StaticAIMLUtils.IsUnknown(unifiable);
        }

        public static string InnerXmlText(XmlNode node)
        {
            return StaticAIMLUtils.InnerXmlText(node);
        }

        internal static bool IsTrueOrYes(string str)
        {
            return StaticAIMLUtils.IsTrueOrYes(str);
        }

        public static bool IsFalseOrNo(string str)
        {
            return StaticAIMLUtils.IsFalseOrNo(str);
        }
        public static bool IsSomething(Unifiable fullPath)
        {
            Unifiable s;
            return TextPatternUtils.IsSomething(fullPath, out s) && s != null;
        }
    }

    [Serializable]
    public abstract class BaseUnifiable : CommonStaticUtils //: StaticAIMLUtils
    {
        public abstract string SpecialName { get; }
        public abstract string OverlyMatchableString { get; }
    }

    public interface IndexTarget
    {
    }

    public interface Indexable
    {
        string OverlyMatchableString { get; }
        string SpecialName { get; }
        bool AddCategory(IndexTarget ci);
        bool RemoveCategory(IndexTarget ci);
    }

    [Serializable]
    public class SpecialStringUnifiable : StringUnifiable
    {
        public SpecialStringUnifiable(string value, string debugName)
            : base(value)
        {
            SpecialCache = debugName;
            upperCache = value;
        }

        public override object Raw
        {
            get { return str; }
        }

        protected override string GenerateSpecialName
        {
            get { return SpecialCache; }
        }

        public override bool SameMeaningCS(Unifiable s, bool caseSensitive)
        {
            if (s is BestUnifiable) return s.SameMeaningCS(this, caseSensitive);
            if (ReferenceEquals(this, s)) return true;
            bool null2 = ReferenceEquals(s, null);
            if (null2) return false;
            if (Equals(SpecialName, s.SpecialName))
            {
                return true;
            }
            if (caseSensitive)
            {
                if (str == s.AsString())
                {
                    return true;
                }
                return false;
            }
            if (ToUpper() == s.ToUpper())
            {
                return true;
            }
            return false;
        }

        public override bool AddCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            return base.AddCategory(template);
            throw new NotImplementedException();
        }

        public override bool RemoveCategory(IndexTarget template)
        {
            if (NOCateIndex) return false;
            throw new NotImplementedException();
        }

        // private readonly string DebugName1;

        public override string ToUpper()
        {
            return AsString();
        }

        public override int RunLowMemHooks()
        {
            return 0;
        }

        public override string AsString()
        {
            return str;
        }

        sealed public override string ToString()
        {
            return AsString();
            return SpecialCache + "-" + DescribeUnifiable(this);
        }

        public override int SpoilCache()
        {
            return 0;
        }

        public override void Append(Unifiable p)
        {
            throw new InvalidCastException("Empty Unifiable");
        }
    }
}