#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;
using Mono.CSharp;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;
using VDS.RDF.Nodes;
using StringWriter = System.IO.StringWriter;
//using TermList = LogicalParticleFilter1.TermListImpl;
//using TermList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;
//using PartList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;

using TermList = LogicalParticleFilter1.SIProlog.PartListImpl;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;
#if MERGED_RDFSTORE
using GraphWithDef = LogicalParticleFilter1.SIProlog.PNode;
#endif

using System.Threading;
//using GraphWithDef = LogicalParticleFilter1.SIProlog.;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;
namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        #region prologObjects

        // Object (of a style...) definitions:
        // Rule = (Head, Body)
        // Head = Term
        // Body = [Term]
        // Term = (id, Parameters)
        // Parameters {Partlist} = [Part]
        // Part = Variable | Atom | Term


        public delegate Part PartReplacer(Part p, PartReplacer pr);

        public static Term MakeTerm(string name, params Part[] parts)
        {
            return new Term(name, false, new PartListImpl(parts));
        }
        public static Term TERM_TRUE
        {
            get
            {
                if (_TERM_TRUE == null)
                {
                    _TERM_TRUE = MakeTerm("true");
                }
                return _TERM_TRUE;
            }
        }

        public static Term RuleBodyToTerm(PartListImpl rulebody)
        {
            if (IsBodyAlwaysTrue(rulebody))
                return TERM_TRUE;
            if (rulebody.Count == 1) return rulebody[0].AsTerm();
            return new Term(",", false, rulebody);
        }
        public static bool IsBodyAlwaysTrue(PartListImpl rulebody)
        {
            return rulebody == null || rulebody.Count == 0 ||
                (rulebody.Count == 1 && TERM_TRUE.Equals(rulebody[0]));
        }
        public static Term RuleToTerm(Part head, PartListImpl rulebody)
        {
            if (IsBodyAlwaysTrue(rulebody))
            {
                return head.AsTerm();
            }
            return MakeTerm(":-", head, RuleBodyToTerm(rulebody));
        }

        static internal SourceLanguage tl_console_language
        {
            get { return threadLocal.tl_console_language ?? SourceLanguage.Text; }
            set { threadLocal.tl_console_language = value; }
        }

        public abstract class Part : IHasParent, IComparable<string>
        {
            public static bool operator ==(Part a, Part b)
            {
                var an = ReferenceEquals(a, null);
                var bn = ReferenceEquals(b, null);
                if (an || bn) return an && bn;
                return a.Equals(b);
            }
            public static bool operator !=(Part a, Part b)
            {
                return !(a == b);
            }
            /// <summary>
            /// Prolog Structure compare '=='/2
            /// </summary>
            /// <param name="term"></param>
            /// <param name="varlist"></param>
            /// <returns></returns>
            public abstract bool SameClause(Part term, IDictionary<string, string> varlist);
            /// <summary>
            /// Like Prolog '=='/2, but it requires variables to have the same identity
            /// </summary>
            /// <param name="obj"></param>
            /// <returns></returns>
            public abstract bool Equals(Part obj);
            sealed public override bool Equals(object obj)
            {
                if (!(obj is Part)) return false;
                return Equals((Part)obj);
            }

            public abstract int GetPlHashCode();
            sealed public override int GetHashCode()
            {
                return GetPlHashCode();
            }
            public virtual bool IsGround
            {
                get { return true; }
            }
            public virtual Part CopyTerm
            {
                get { return this; }
            }
            protected Exception Missing(string p)
            {
                return ErrorBadOp("{0} Missing '{1}' for {2}", type, p, this.ToSource(tl_console_language));
            }
            public abstract string type { get; }

            virtual public object Functor0
            {
                get { throw Missing("Functor"); }
            }
            public virtual int Arity
            {
                get { throw Missing("Arity"); }
            }
            public virtual PartListImpl ArgList
            {
                get { throw Missing("ArgList"); }
            }
            public virtual IHasParent TParent
            {
                set { }
                get { return null; }
            }

            public virtual void print(Action<string> w)
            {
                w(ToSource(tl_console_language));
            }

            public virtual string Text
            {
                get
                {
                    return ToSource(SourceLanguage.Text);
                }
            }
            public abstract string ToSource(SourceLanguage language);
            /// <summary>
            /// Returns a string representation readable with PartPart(..);
            /// </summary>
            public virtual string StringReadable
            {
                get
                {
                    return ToSource(SourceLanguage.Prolog);
                }
            }
            /// <summary>
            /// Returns URI as their Turtle expression as found in <>
            /// Literals/Strings as their Values without quotes
            /// BNodes as their _ID
            /// Variables as their ID
            /// Terms as their Functor
            /// </summary>
            /// <returns></returns>
            virtual public string vname
            {
                get { throw Missing("VarName"); }
            }
            virtual public string fname
            {
                get { throw Missing("FunctorName"); }
            }
            virtual public string fvname
            {
                get { throw Missing("FunctorVarName"); }
            }

            public virtual bool IsObject
            {
                get { return false; }
            }

            /// <summary>
            /// Returns a string representation probably not readable with PartPart(..);
            /// but makes sense to humans:
            ///  Atoms: name
            ///  Variables: varname
            /// </summary>
            sealed public override string ToString()
            {
                return ToSource(tl_console_language);
            }

            public virtual double AsDouble()
            {
                throw Missing("AsDouble");
            }

            public static void ConsolePrint(string format, params object[] args)
            {
                Console.Write(format, args);
            }

            public virtual void Visit(PartReplacer replacer)
            {
                return;
            }

            public virtual string AsString()
            {
                throw Missing("AsString");
            }

            public abstract Term AsTerm();

            #region IComparable<string> Members

            int IComparable<string>.CompareTo(string other)
            {
                return Text.CompareTo(other);
            }

            #endregion
        }

        public class PartListImpl : Part, IEnumerable<Part>, IHasParent
        {
            public override Term AsTerm()
            {
                return RuleBodyToTerm(this);
            }
            //private string fuctor;
            public override IHasParent TParent
            {
                get
                {
                    var p = parent;
                    if (p != null && p.TParent != null)
                    {
                        p = p.TParent;
                    }
                    return p;
                }
                set
                {
                    parent = value;
                }
            }
            internal IHasParent parent;
            // Parameters {Partlist} = [Part]
            // Part = Variable | Atom | Term
            //public string name;

            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as PartListImpl;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                int i = 0;
                foreach (var s in this)
                {
                    if (!term2[i++].SameClause(s, varlist)) return false;
                }
                return true;
            }

            public override bool Equals(Part term)
            {
                var term2 = term as PartListImpl;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                int i = 0;
                foreach (var s in this)
                {
                    if (!term2[i++].Equals(s)) return false;
                }
                return true;
            }

            public override int GetPlHashCode()
            {
                return Arity;
            }

            override public bool IsGround
            {
                get
                {
                    foreach (var t in this.tlist)
                    {
                        if (t is Atom) continue;
                        if (!t.IsGround)
                        {
                            return false;
                        }
                        if (t is Term)
                        {
                            return false;
                        }
                    }
                    return true;
                }
            }

            public override Part CopyTerm
            {
                get
                {
                    var pl = new PartListImpl();
                    foreach (var t in this)
                    {
                        pl.AddPart(t.CopyTerm);
                    }
                    return pl;
                }
            }

            public override string type { get { return "PartList"; } }
            private readonly IList<Part> tlist;
            public int renumber = 0;
            public int Count
            {
                get { return tlist.Count; }
            }

            public Part this[int i]
            {
                get
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    return (SIProlog.Part)tlist[i];
                }
                set
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    tlist[i] = value;
                }
            }
            public Part this[int i, bool safe]
            {
                get
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    return (SIProlog.Part)tlist[i];
                }
                set
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    tlist[i] = value;
                }
            }
            public void Add(SIProlog.Part part)
            {
                if (part is SIProlog.PartListImpl)
                {
                    throw ErrorBadOp("inner partlist");
                }
                tlist.Add(part);
            }

            public void Insert(int i, SIProlog.Part part)
            {
                if (i == Count)
                {
                    Add(part);
                    return;
                }
                throw ErrorBadOp("inserting outof order");
            }

            override public int Arity
            {
                get
                {
                    return tlist.Count;
                }
            }
            public override PartListImpl ArgList
            {
                get
                {
                    return this;
                }
            }
            public Part a1
            {
                get
                {
                    if (Arity < 1) return null;
                    return tlist[0];
                }
            }
            public Part a2
            {
                get
                {
                    if (Arity < 2) return null;
                    return tlist[1];
                }
            }
            //public PartList(string head) { name = head; }
            public PartListImpl(params Part[] lS)
            {
                this.tlist = new List<Part>(lS);
            }
            public PartListImpl(IList<Part> parts)
            {
                this.tlist = parts;
            }

            public PartListImpl()
            {
                this.tlist = new List<Part>();
            }

            public PartListImpl(PartListImpl head, PEnv env)
            {
                this.tlist = new List<Part>();
                for (var i = 0; i < head.Count; i++)
                {
                    tlist.Add(value(head[i], env));
                }

            }

            public override void print(Action<string> writer)
            {
                bool com = false;
                // ConsoleWrite("plist(");
                foreach (Part p in tlist.ToList())
                {
                    if (com) writer(", ");
                    p.print(writer);
                    com = true;
                }
                //  ConsoleWrite(")");
            }
            public override string ToSource(SourceLanguage language)
            {
                string result = "";
                bool com = false;
                //result = "plist(";
                foreach (Part p in tlist)
                {
                    if (com) result += (", ");
                    result += p.ToSource(language);
                    com = true;
                }
                // result += ")";
                return result;
            }

            public void AddPart(Part term)
            {
                tlist.Add(term);
            }

            public void InsertPart(int i, Part part)
            {
                if (i != tlist.Count)
                {
                    throw ErrorBadOp("out of order insertion");
                }
                tlist.Insert(i, part);
            }
            public override void Visit(PartReplacer func)
            {
                int argNum = 0;
                foreach (var arg in tlist.ToArray())
                {
                    var argr = func(arg, func);
                    if (ReferenceEquals(argr, null)) return;
                    if (!ReferenceEquals(argr, arg))
                    {
                        this.ArgList[argNum] = argr;
                    }
                    argNum++;
                }
            }

            public IEnumerator<Part> GetEnumerator()
            {
                return tlist.GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            #region IList<Part> Members

            public int IndexOf(Part item)
            {
                throw new NotImplementedException();
            }

            public void RemoveAt(int index)
            {
                throw new NotImplementedException();
            }

            #endregion

            #region ICollection<Part> Members


            public void Clear()
            {
                throw new NotImplementedException();
            }

            public bool Contains(Part item)
            {
                throw new NotImplementedException();
            }

            public void CopyTo(Part[] array, int arrayIndex)
            {
                throw new NotImplementedException();
            }

            public bool IsReadOnly
            {
                get { throw new NotImplementedException(); }
            }

            public bool Remove(Part item)
            {
                throw new NotImplementedException();
            }

            #endregion
        }

        public static Dictionary<String, Atom> AtomTable = new Dictionary<String, Atom>();

        public interface IAtomic
        {
            int CompareTo(IAtomic atomic);
            INode AsRDFNode();
            /// <summary>
            /// Tries if possible to return a IValuedNJode
            /// </summary>
            /// <returns></returns>
            IValuedNode AsValuedNode();
            string AsString();
            bool Unify(IAtomic atomic);
            double AsDouble();
            object Functor0 { get; }
            bool IsNode { get; }
        }


        public class Variable : Part
        {
            public override Term AsTerm()
            {
                return new Term(vname, true, new PartListImpl());
            }

            public string _name;
            public override string vname { get { return _name; } }
            public bool Anonymous = false;
            public override string type { get { return "Variable"; } }
            public Variable(string head)
            {
                if (head == null || head == "_")
                {
                    Anonymous = true;
                    head = "ANON_" + CONSP;
                }
                _name = head;
            }
            public override void print(Action<string> w) { w(ToSource(tl_console_language)); }
            public override string ToSource(SourceLanguage language)
            {
                if (language != SourceLanguage.Prolog) return "?" + this.vname;
                if (Anonymous) return "_";
                return this.vname;
            }

            override public bool IsGround
            {
                get { return false; }
            }
            public override bool Equals(Part term)
            {
                if (ReferenceEquals(this, term)) return true;
                var term2 = term as Variable;
                if (term2 == null) return false;
                string thisname = this.vname;
                string term2name = term2.vname;
                if (term2name == thisname)
                {
                    return true;
                }
                return false;
            }
            public override int GetPlHashCode()
            {
                return vname.GetHashCode();
            }
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                if (ReferenceEquals(this, term)) return true;
                var term2 = term as Variable;
                if (term2 == null) return false;
                if (varlist == null)
                {
                    if (Anonymous) return term2.Anonymous;
                    return false;
                }
                return SameVar(term2.vname, this.vname, varlist);
            }
        }
        public static bool SameVar(string term2name, string thisname, IDictionary<string, string> varlist)
        {
            if (term2name == thisname) return true;
            string thisothername;
            if (!varlist.TryGetValue(thisname, out thisothername))
            {
                if (varlist.ContainsKey(term2name)) return false;
                varlist[thisname] = term2name;
                varlist[term2name] = thisname;
                return true;
            }
            return thisothername == term2name;
        }
        public static bool IsListName(string s)
        {
            return s == "cons" || s == "." || s == FUNCTOR_CONS;
        }
        public static bool IsList(Part x)
        {
            return x is Term && IsListName(((Term)x).fname) && x.Arity == 2;
        }
        public interface IHasParent
        {
            IHasParent TParent { get; set; }
        }
        public class Term : Part, IHasParent
        {
            public override Term AsTerm()
            {
                return this;
            }
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as Term;
                if (term2 == null) return false;
                if (headIsVar)
                {
                    if (!term2.headIsVar) return false;
                    if (!SameVar(term2.fname, this.fname, varlist)) return false;
                }
                else
                {
                    if (term2.headIsVar) return false;
                    if (term2.fname != this.fname) return false;
                }
                return ArgList.SameClause(term2.ArgList, varlist);
            }
            public override bool Equals(Part term)
            {
                var term2 = term as Term;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                if (term2.headIsVar != headIsVar) return false;
                if (term2.fname != this.fname) return false;
                return ArgList.Equals(term2.ArgList);
            }
            public override int GetPlHashCode()
            {
                return fname.GetHashCode() ^ Arity;
            }
            //readonly public string _name;
            //public override string name { get { return _name; } }
            public override string type { get { return "Term"; } }

            public override PartListImpl ArgList
            {
                get { return partlist0.ArgList; }
            }

            override public int Arity
            {
                get
                {
                    if (partlist0 == null)
                    {
                        return base.Arity;
                    }
                    return partlist0.Arity;
                }
            }

            override public bool IsGround
            {
                get { return partlist0.IsGround && !headIsVar; }
            }
            public override Part CopyTerm
            {
                get
                {
                    return new Term(headIsVar ? fvname : fname, headIsVar, (PartListImpl) partlist0.CopyTerm)
                               {parent = null, excludeThis = excludeThis};
                }
            }
            private Part _pred = null;
            public readonly PartListImpl partlist0;
            public bool excludeThis = false;
            public int excludeRule = -1;
            public bool cut = false;
            public IHasParent parent = null;
            public override string fname
            {
                get
                {
                    return _name;
                }
            }
            public override string fvname
            {
                get
                {
                    if (headIsVar) return _name;
                    return base.fvname;
                }
            }
            /// <summary>
            /// If the entire term is a var
            /// </summary>
            public override string vname
            {
                get
                {
                    if (Arity == 0) return fvname;
                    return base.vname;
                }
            }
            private string _name;
            public bool headIsVar;
            public Term(string head, bool isVar, PartListImpl a0N)
            {
                _name = head;
                headIsVar = isVar;
                partlist0 = a0N;
                var isVarName = IsVarName(head);
                if (a0N == null)
                {
                    throw ErrorBadOp("Term Arglist NULL: {0}", head);
                }
                if (isVar)
                {
                    if (!isVarName) throw ErrorBadOp("Pred was supposed to be variable: {0}", this);
                }
                else
                {
                    if (isVarName) throw ErrorBadOp("Pred was NOT supposed to be variable: {0}", this);
                }
                if (a0N.Arity > 0)
                {
                    Part a0 = a0N.ArgList[0];
                    if (a0 is PartListImpl)
                    {
                        Warn("Poorly constructed term: {0}", this);
                    }
                }
                a0N.TParent = this;
            }


            public override string ToSource(SourceLanguage language)
            {
                language = language.Inner();
                string result = "";
                if (IsListName(this.fname) && Arity == 2)
                {
                    Part x = this;
                    {
                        result += "[";
                        var com = false;
                        while (IsList(x))
                        {
                            if (com) result += ", ";
                            result += x.ArgList[0].ToSource(language); // May need to case var/atom/term
                            com = true;
                            x = x.ArgList[1];
                        }
                        if (x.ToSource(SourceLanguage.Prolog) != "[]")
                        {
                            result += " | ";
                            result += x.ToSource(language);
                        }
                        result += "]";
                        return result;
                    }
                }
                if (Arity == 0)
                {
                    if (fname == "cut")
                    {
                        return "!";
                    }
                    return ReadableName;
                }
                PartListImpl argList = this.ArgList;
                if (argList == null)
                {
                    return result + "()";
                }
                result += "" + ReadableName + "(";
                result += argList.ToSource(language);
                result += ")";
                return result;
            }

            protected string ReadableName
            {
                get
                {
                    if (headIsVar) return fvname;
                    var name = this.fname;
                    if (IsVarName(name)) return "'" + name + "'";
                    return name;
                }
            }

            override public void Visit(PartReplacer func)
            {
                ArgList.Visit(func);
                /*
                int argNum = 0;
                foreach (var arg in Args)
                {
                    var argr = func(arg, func);
                    if (ReferenceEquals(argr, null)) return;
                    if (!ReferenceEquals(argr, arg))
                    {
                        this.ArgList[argNum] = argr;
                    }
                    argNum++;
                }
               */
            }

            #region IHasParent Members

            public override IHasParent TParent
            {
                get
                {
                    return parent;
                }
                set
                {
                    parent = value;
                }
            }

            #endregion

            public Part a1
            {
                get { return ArgList.a1; }
            }
            public Part a2
            {
                get { return ArgList.a2; }
            }
        }

        public static bool IsVarName(string name)
        {
            // should be [A-Z\_\?]
            if (name.Length == 0) return false;
            char firstChar = name[0];
            if (firstChar == '?' || firstChar == '_')
            {
                return true;
            }
            return Char.IsLetter(firstChar) && Char.IsUpper(firstChar);
        }

        #endregion

    }

    public class KeyCase : IEqualityComparer<string>
    {
        public static KeyCase Default = new KeyCase(NormalizeKeyLowerCase);
        public static KeyCase DefaultFN = new KeyCase(NormalizeKeyLowerCaseNoFileExt);
        #region Implementation of IEqualityComparer<string>

        public Func<object, string> NormalizeKey;
        static public readonly char[] RegexMarkers = "$^*[|]".ToCharArray();

        public KeyCase(Func<object, string> normalizer)
        {
            NormalizeKey = normalizer;
        }
        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.
        ///                 </param><param name="y">The second object of type <paramref name="T"/> to compare.
        ///                 </param>
        public bool Equals(string x, string y)
        {
            return SameKey(x, y);
        }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.
        ///                 </param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.
        ///                 </exception>
        public int GetHashCode(string obj)
        {
            if (ContainsRegex(obj))
            {
                throw new InvalidOperationException("Keys should not contain Regex!?! " + obj);
            }
            return NormalizeKey(obj).GetHashCode();
        }

        public static string NormalizeKeyLowerCase(object s)
        {
            string fn = s.ToString().Trim().ToLower().Replace(" ", "_");
            fn = fn.Trim('.', '_', ' ');
            return fn;// Path.GetFileNameWithoutExtension(fn);
        }
        public static string NormalizeKeyLowerCaseNoFileExt(object s)
        {
            string fn = s.ToString().Trim().ToLower().Replace(" ", "_");
            fn = fn.Trim('.', '_', ' ');
            return Path.GetFileNameWithoutExtension(fn);
        }
        #endregion

        public bool SameKeyO(object cK, object cP)
        {
            if (Equals(cK, cP)) return true;
            if (cK.GetType().IsValueType)
            {
                throw new InvalidOperationException("lcase " + cK.GetType());
            }
            return SameKey(cK.ToString(), cP.ToString());
        }
        public bool SameKey(string cK, string cP)
        {
            return Compare(cK, cP) == 0;
        }
        public int Compare(string cK, string cP)
        {
            if (cK == cP) return 0;
            var cnK = NormalizeKey(cK);
            var cnP = NormalizeKey(cP);
            bool crK = ContainsRegex(cK);
            bool crP = ContainsRegex(cP);
            if (cnK == cnP) return 0;
            if (!crK && !crP)
            {
                return cnK.CompareTo(cnP);
            }
            if (crK && crP)
            {
                return cnK.CompareTo(cnP);
            }
            if (crK)
            {
                var swap = cK;
                cK = cP;
                cP = swap;
            }
            if (Regex.IsMatch(cK, cP, RegexOptions.IgnoreCase))
            {
                return 0;
            }
            return cnK.CompareTo(cnP);
        }

        private static bool ContainsRegex(string c1)
        {
            if (c1.IndexOfAny(RegexMarkers) < 0)
            {
                return false;
            }
            return true;
        }
    }

    public class CIDictionary<K, V> : Dictionary<K, V>
    {
        public KeyCase myComp
        {
            get
            {
                return (KeyCase)base.Comparer;
            }
        }

        public CIDictionary()
            : base((IEqualityComparer<K>)KeyCase.Default)
        {

        }
        public CIDictionary(IEqualityComparer<K> comp)
            : base(comp)
        {

        }
        public CIDictionary(IDictionary<K, V> dict)
            : base(dict, (IEqualityComparer<K>)KeyCase.Default)
        {

        }
        public CIDictionary(IDictionary<K, V> dict, IEqualityComparer<K> comp)
            : base(dict, comp)
        {

        }

        public V this[K key, Func<V> ifMissing]
        {
            get
            {
                V v;
                if (TryGetValue(key, out v))
                {
                    return v;
                }
                return ifMissing();
            }

            set
            {
                string key1 = "" + key;
                var key2 = this.myComp.NormalizeKey(key);
                if (key2 != key1)
                {
                    //throw BadOp();
                }
                base[key] = value;
            }
        }
    }
}

