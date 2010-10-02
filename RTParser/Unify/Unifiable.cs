using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using MushDLR223.Utilities;
using RTParser.Database;
using RTParser.Utils;
using UPath = RTParser.Unifiable;
using StringAppendableUnifiable = RTParser.StringAppendableUnifiableImpl;
//using StringAppendableUnifiable = System.Text.StringBuilder;

namespace RTParser
{
    abstract public class Unifiable : StaticAIMLUtils, IConvertible, IComparable<Unifiable>
    {
        static Unifiable()
        {
            StaticXMLUtils.FormatProviderConvertor = FormatProviderConvertor0;
        }

        public static IConvertible FormatProviderConvertor0(IConvertible arg, Type solid)
        {
            IConvertible output = arg;
            if (Object.ReferenceEquals(arg, null))
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
                string u = arg.ToString(FormatProvider);

                if (solid == typeof (Unifiable))
                {
                    return (Unifiable) u;
                }
                if (solid == typeof (string))
                {
                    return u;
                }
                if (solid == typeof (Double))
                {
                    return Double.Parse(u);
                }
                if (solid == typeof (Int32))
                {
                    return Int32.Parse(u);
                }
            }
            catch (Exception exception)
            {
                writeToLog("ERROR FormatProviderConvertor " + arg + " to " + solid);
            }
            //if (FormatProvider == null) FormatProvider = ;//.;//new IFormatProvider();
            var format = FormatProvider.GetFormat(solid);
            object oo = arg.ToType(solid, FormatProvider);
            return (IConvertible) oo;
        }


        public UFlags Flags = UFlags.NO_FLAGS;
        public bool IsFlag(UFlags f)
        {
            return (f & Flags) != 0;
        }

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

        public const float UNIFY_TRUE = 0;
        public const float UNIFY_FALSE = 1;


        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }

        public static implicit operator string(Unifiable value)
        {
            if (ReferenceEquals(value, null))
            {
                return null;
            }
            return value.AsString();
        }

        static Dictionary<string, Unifiable> internedUnifiables = new Dictionary<string, Unifiable>(20000);
        public static implicit operator Unifiable(string value)
        {
            return MakeStringUnfiable(value);
        }

        private static StringUnifiable MakeStringUnfiable(string value)
        {
            if (value == null) return null;
            Unifiable u;
            if (false)
            {
                u = new StringUnifiable(value);
                return (StringUnifiable)u;
            }
            if (true)
                lock (internedUnifiables)
                {
                    if (internedUnifiables.TryGetValue(value, out u))
                    {
                        return (StringUnifiable)u;
                    }
                }

            string key = AIMLLoader.CleanWhitepaces(value);
            if (value != key)
            {
                //   writeToLog("Triming? '" + value + "'");
                value = key;
                //return new StringUnifiable(value);
            }
            if (true)
                lock (internedUnifiables)
                {
                    if (!internedUnifiables.TryGetValue(key, out u))
                    {
                        u = internedUnifiables[key] = new StringUnifiable(value, true);
                        // ReSharper disable ConditionIsAlwaysTrueOrFalse
                        if (false && (internedUnifiables.Count % 10000) == 0)
                        // ReSharper restore ConditionIsAlwaysTrueOrFalse
                        {
                            writeToLog("DEBUG9 internedUnifiables.Count=" + internedUnifiables.Count);
                        }
                    }
                    return (StringUnifiable)u;
                }
            u = new StringUnifiable(value);
            return (StringUnifiable)u;
        }

        public static Unifiable Empty = new EmptyUnifiable();


        public static Unifiable STAR
        {
            get
            {
                return MakeStringUnfiable("*");
            }
        }


        public string ToMatchPattern()
        {
            return ToUpper();
        }

        public static Unifiable Join(string sep, Unifiable[] values, int startIndex, int count)
        {
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, FromArrayOf(values), startIndex, count);
        }

        public static Unifiable Join(string sep, string[] values, int startIndex, int count)
        {
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, values, startIndex, count);
        }

        public static Unifiable[] arrayOf(string[] strs)
        {
            Unifiable[] it = new Unifiable[strs.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = Unifiable.Create(strs[i].Trim());
            }
            return it;
        }

        public static string[] FromArrayOf(Unifiable[] tokens)
        {
            string[] it = new string[tokens.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = tokens[i].AsString().Trim();
            }
            return it;
        }

        static public bool operator ==(Unifiable t, Unifiable s)
        {
            if (IsNull(t))
            {
                return IsNull(s);
            }
            if (IsNull(s))
            {
                return false;
            }
            if (IsMissing(t))
            {
                return IsMissing(s);
            }
            if (t.ToUpper() == s.ToUpper()) return true;
            return false;
            if (t.ToValue(null).ToLower() == s.ToValue(null).ToLower())
            {
                writeToLog("==({0},{1})", t.AsString(), s.AsString());
                return false;
            }

            return false;
        }

        public static bool operator !=(Unifiable t, Unifiable s)
        {
            return !(t == s);
        }

        public static Unifiable operator +(string u, Unifiable more)
        {
            if (u.Length == 0) return more;
            if (more == null) return u + " -NULL-";
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0) return u;
            return MakeStringUnfiable(u + more.AsString());
        }
        public static Unifiable operator +(Unifiable u, string more)
        {
            return MakeStringUnfiable("" + u.AsString() + more);
        }
        public static Unifiable operator +(Unifiable u, Unifiable more)
        {
            if (IsNullOrEmpty(more)) return u;
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0) return u;
            return MakeStringUnfiable(u.AsString() + " " + moreAsString);
        }

        public static Unifiable Create(object p)
        {
            if (p is string)
            {
                return MakeStringUnfiable((string)p);
            }
            if (p is Unifiable) return (Unifiable)p;
            if (p is XmlNode)
            {
                var n = (XmlNode)p;
                string inner = InnerXmlText(n);
                writeToLog("MAking XML Node " + n.OuterXml + " -> " + inner);
                StringUnifiable unifiable = MakeStringUnfiable(inner);
                //unifiable.node = (XmlNode)p;
            }
            // TODO
            if (p == null)
            {
                return null;
            }
            return MakeStringUnfiable(p.ToString());
        }

        public override string ToString()
        {
            writeToLog("ToSTring");
            return GetType().Name + "={" + Raw + "}";//Raw.ToString();}
        }

        internal static StringAppendableUnifiable CreateAppendable()
        {
            return new StringAppendableUnifiable();
            //   return new StringBuilder(10);
        }

        public static Unifiable ThatTag = Create("TAG-THAT");
        public static Unifiable TopicTag = Create("TAG-TOPIC");
        public static Unifiable FlagTag = Create("TAG-FLAG");
        public static Unifiable InputTag = Create("TAG-INPUT");
        public static Unifiable TagStartText = Create("TAG-START");
        public static Unifiable TagEndText = Create("TAG-END");

        public abstract object Raw { get; }
        public virtual bool IsEmpty
        {
            get
            {
                string s = AsString();
                if (String.IsNullOrEmpty(s)) return true;
                s = s.Trim();
                if (s.Length != 0)
                {
                    // writeToLog("was IsEmpty");
                    return false;
                }
                return true;
            }
        }

        public Unifiable LegacyPath
        {
            get { return this; }
        }

        public virtual bool IsUnitMatcher
        {
            get { return IsShort(); }
        }

        public virtual bool IsStarMatcher
        {
            get { return !IsUnitMatcher && IsWildCard(); }
        }

        public bool CanMatchZero
        {
            get { return Raw != null && ToUpper().Length == 0; }
        }

        public virtual bool IsFalse()
        {
            return IsEmpty;
        }
        public abstract bool IsTag(string s);
        public virtual bool IsWildCard()
        {
            return true;
        }
        public abstract bool IsLazyStar();
        public abstract bool IsLongWildCard();
        public abstract bool IsFiniteWildCard();

        public abstract bool IsLazy();
        public abstract bool IsLitteral();
        public abstract bool IsLitteralText();

        public static void writeToLog(string message, params object[] args)
        {
            try
            {
                RTPBot.writeDebugLine("UNIFYABLETRACE: " + message, args);
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
            string su = ToUpper();
            if (su == "*") return !other.IsEmpty;
            if (su == "_") return other.IsShort();
            return Unify(other, query) == UNIFY_TRUE;
        }

        public bool CanUnify(Unifiable other, SubQuery query)
        {
            string su = ToUpper();
            if (su == "*") return !other.IsEmpty;
            if (su == "_") return other.IsShort();
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

        static public bool IsMatch2(string st, string actualValue)
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
                var b = NatLangDb.IsWordClass(actualValue, type);
                if (b)
                {
                    return true;
                }
                return b;
            }
            return false;
        }


        static bool TwoSemMatch(string that, string thiz)
        {
            return false;
            if (NatLangDb.IsWordClassCont(that, " determ") && NatLangDb.IsWordClassCont(thiz, " determ"))
            {
                return true;
            }
            return false;
        }

        public static bool MustBeFast = true;
        static bool TwoMatch0(string s1, string s2)
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


        public static Unifiable NULL = new StringUnifiable(null);

        public static Unifiable FAIL_NIL = new StringUnifiable("NIL");

        public virtual Unifiable ToCaseInsenitive()
        {
            return this;
        }
        public virtual Unifiable Frozen(SubQuery subquery)
        {
            return ToValue(subquery);
        }
        public abstract string ToValue(SubQuery subquery);
        public abstract string AsString();
        public virtual Unifiable ToPropper()
        {
            return this;
        }
        public virtual Unifiable Trim()
        {
            return this;
        }

        //public abstract Unifiable[] Split(Unifiable[] unifiables, StringSplitOptions options);
        //public abstract Unifiable[] Split();


        // join functions
        public abstract void Append(Unifiable part);
        public abstract void Append(string part);
        public abstract void Clear();

        public abstract Unifiable First();

        public abstract Unifiable Rest();

        public abstract bool IsShort();

        public abstract object AsNodeXML();

        public static Unifiable MakePath(Unifiable unifiable)
        {
            return unifiable;
        }

        public abstract Unifiable[] ToArray();

        virtual public bool StoreWildCard()
        {
            return true;
        }

        public static string ToVMString(object unifiable)
        {
            if (unifiable is Unifiable)
            {
                return ((Unifiable)unifiable).AsString();
            }
            if (ReferenceEquals(null, unifiable))
            {
                return "-NULL-";
            }
            return "" + unifiable;
        }

        public abstract bool IsAnySingleUnit();

        public abstract string ToUpper();

        public bool IsAnyWord()
        {
            return IsUnitMatcher;
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

        public abstract double Strictness();
        public abstract int CompareTo(Unifiable other);

        public static string DescribeUnifiable(Object value)
        {
            if (value == null) return "-NULL-UOBJECT-";
            if (ReferenceEquals(value, Unifiable.NULL)) return "-EQ-NULL-";
            if (IsNull(value)) return "-EQUAL-NULL-";
            if (IsMissing(value)) return "-EQUAL-MISSING-" + value + "-";
            return "" + value;

        }
    }
}

