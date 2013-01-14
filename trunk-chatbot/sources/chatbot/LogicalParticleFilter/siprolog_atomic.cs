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

using System.Threading;
//using GraphWithDef = LogicalParticleFilter1.SIProlog.;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;
namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        public static NamespaceMapper rdfDefNS
        {
            get { return (NamespaceMapper)rdfDefinations.NamespaceMap; }
        }
        public class Atom : Part, IAtomic
        {

            public static bool operator ==(Atom a, Atom b)
            {
                return NodeEquality(a.AsRDFNode(), b.AsRDFNode());
            }

            public static bool operator !=(Atom a, Atom b)
            {
                return !(a == b);
            }

            public static bool NodeEquality(INode x, INode y)
            {
                return x.Equals(y);
            }

            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as Atom;
                if (ReferenceEquals(term2, null)) return false;
                return this.Unify(term2);
            }

            public Object objRef
            {
                get
                {
                    if (_objRef == null)
                    {
                        _objRef = MakeNodeInside(prefixNs, localValue, quoted);
                    }
                    return _objRef;
                }
            }
            object _objRef;
            string localValue;
            string prefixNs;
            string stringReadableCache;

            static string GetNamespaceUri(string ns)
            {
                if (ns.Contains(":/")) return ns;
                if (ns.EndsWith(":"))
                {
                    ns = ns.Substring(0, ns.Length - 1);
                }
                if (rdfDefNS.HasNamespace(ns)) return rdfDefNS.GetNamespaceUri(ns).AbsoluteUri;
                return ns;
            }
            static string GetNamespacePrefix(string ns)
            {
                if (ns.Contains(":/"))
                {
                    string prefixC = null;
                    foreach (var prefix in rdfDefNS.Prefixes)
                    {
                        var uri = rdfDefNS.GetNamespaceUri(prefix).AbsoluteUri;
                        if (uri == ns) return prefix;
                        if (uri.Length > ns.Length)
                        {
                            if (!uri.StartsWith(ns)) continue;
                        }
                        if (ns.Length > uri.Length)
                        {
                            if (!ns.StartsWith(uri)) continue;
                        }
                        prefixC = prefix;
                    }
                    if (prefixC != null) return prefixC;
                    string qname;
                    string ln = "name";
                    if (rdfDefNS.ReduceToQName(ns + ln, out qname))
                    {
                        prefixC = qname.Substring(0, qname.Length - ln.Length);
                    }
                    if (prefixC != null) return prefixC;
                }
                if (ns.EndsWith(":"))
                {
                    ns = ns.Substring(0, ns.Length - 1);
                }
                return ns;
            }

            public string NamespaceUri
            {
                get
                {
                    string ns = NamespaceX();
                    return GetNamespaceUri(ns);
                }
            }
            public string Namespace
            {
                get
                {
                    string ns = NamespaceX();
                    return GetNamespacePrefix(ns);
                }
            }
            public string NamespaceX()
            {
                string localAname = prefixNs;
                if (!IsUri) return localAname;
                //if (localAname != null) return localAname;
                string s = AsValuedNode().AsString();
                string oprefix, ouri, path = s;
                if (!GraphWithDef.DevolveURI(rdfDefNS, path, out ouri, out oprefix,
                                             out localAname, true, false)) return null;
                var ns = oprefix ?? ouri ?? prefixNs;
                oprefix = string.Intern(ns);
                return oprefix;

            }
            public string LocalName
            {
                get
                {
                    string localAname = localValue;
                    if (!IsUri) return localAname;
                    //if (localAname != null) return localAname;
                    string s = AsValuedNode().AsString();
                    string oprefix, ouri, path = s;
                    if (!GraphWithDef.DevolveURI(rdfDefNS, path, out ouri, out oprefix,
                                                 out localAname, true, false))
                    {
                        return null;
                    }
                    if (localAname == null)
                    {
                        return null;
                    }
                    localAname = string.Intern(localAname);
                    return localAname;
                }
            }
            private Func<object, object> Functor0Function;
            public override string name
            {
                get
                {                    
                    if (IsString)
                    {
                        return localValue;
                    }
                    if (localValue == "") return NamespaceUri;
                    var localName = LocalName ?? this.localValue;
                    if (!string.IsNullOrEmpty(localName)) return localName;
                    var f = "" + Functor0;
                    var fi = string.Intern(f);
                    return fi;
                }
            }
            public object Functor0
            {
                get
                {
                    if (Functor00 == null)
                    {
                        Functor00 = Functor000;
                    }
                    return Functor00;
                }
            }
            private object Functor00;
            public object Functor000
            {
                get
                {
                    if (Functor0Function != null) return Functor0Function(objRef);
                    return objRef;
                }
            }
            static public object INodeToObject(object obj)
            {
                var _name = ToValueNode((INode)obj);
                if (!(_name is IValuedNode))
                {
                    var vnode = _name.AsValuedNode();
                    if (ReferenceEquals(null, vnode))
                    {
                    }
                    else
                    {
                        if (!ReferenceEquals(vnode, _name))
                        {
                            _name = vnode;
                        }
                    }
                }
                {
                    if (_name is StringNode)
                    {
                        return _name.AsValuedNode().AsString();
                    }
                    if (_name is BooleanNode)
                    {
                        return _name.AsValuedNode().AsBoolean();
                    }
                    if (_name is DateTimeNode)
                    {
                        return ((DateTimeOffset)_name.AsValuedNode().AsDateTime()).DateTime;
                    }
                    if (_name is NumericNode)
                    {
                        if (_name is LongNode)
                        {
                            return _name.AsValuedNode().AsInteger();
                        }
                        if (_name is UnsignedLongNode)
                        {
                            return (ulong)_name.AsValuedNode().AsInteger();
                        }
                        if (_name is SignedByteNode)
                        {
                            return (sbyte)_name.AsValuedNode().AsInteger();
                        }
                        if (_name is DoubleNode)
                        {
                            return _name.AsValuedNode().AsDouble();
                        }
                        if (_name is DecimalNode)
                        {
                            return _name.AsValuedNode().AsDecimal();
                        }
                        if (_name is FloatNode)
                        {
                            return _name.AsValuedNode().AsFloat();
                        }
                        if (_name is ByteNode)
                        {
                            return (byte)_name.AsValuedNode().AsInteger();
                        }
                    }
                    if (!(_name is IUriNode))
                    {
                        if (_name is IBlankNode)
                        {
                            return _name;
                        }
                        if (_name is ILiteralNode)
                        {
                            var vnode = _name.AsValuedNode();
                            return vnode.AsString();
                        }
                        return _name;
                    }
                    string localAname;
                    //if (aname != null) return aname;
                    string path = _name.AsValuedNode().AsString();
                    string prefix, uri;
                    bool devolved = GraphWithDef.DevolveURI(rdfDefNS, path, out uri, out prefix,
                                                            out localAname, true, false);
                    bool noaname = string.IsNullOrEmpty(localAname);
                    if (devolved && !noaname)
                    {
                        if (string.IsNullOrEmpty(prefix))
                        {
                            return path;
                        }
                        return localAname;
                    }
                    return path;
                }
            }
            private int? hash_code;
            public int hash { get { return GetHashCode(); } }
            public string quoted = null;
            public override string type { get { return "Atom"; } }
            public Atom(string prefix, string name, string quoteType)
            {
                if (false && string.IsNullOrEmpty(name) && quoteType != SYNTAX_DoubleQuotes)
                {
                    name = prefix;
                    prefix = "+";
                }
                prefixNs = prefix;
                localValue = name;
                quoted = quoteType;
                hash_code = this.name.GetHashCode();
                SanityCheck();
                AddToAtomTable();
            }
            public Atom(INode head)
            {
                if (Init((INode)head))
                {
                    AddToAtomTable();
                    hash_code = name.GetHashCode();
                }
            }

            private void AddToAtomTable()
            {
                if (localValue == "C_AKJ315005-tax")
                {
                }
                lock (AtomTable) AtomTable[MakeAtomKey(prefixNs, localValue, quoted)] = this;
            }

            private bool SanityCheck()
            {
                if (RdfDeveloperSanityChecks <= 1) return true;
                var name = this.name;

                if (ReferenceEquals(null, PrologEmptyString)) return true;
                if (ReferenceEquals(this, PrologEmptyString)) return true;
                if (prefixNs == "+")
                {
                }
                do
                {
                    if (string.IsNullOrEmpty(localValue) //|| string.IsNullOrEmpty(quoted)
                        || string.IsNullOrEmpty(prefixNs)
                        || string.IsNullOrEmpty(name)
                        || string.IsNullOrEmpty(StringReadable))
                    {
                        if (IsLiteral)
                        {
                            if (string.IsNullOrEmpty(prefixNs)) break;
                        }
                        if (prefixNs == NamespaceUri)
                        {
                            break;
                        }
                        Warn("Imporoper Atom");
                        return false;
                    }
                    break;
                } while (true);
                var localAname = "" + INodeToObject(objRef);
                string s = ToSource(SourceLanguage.Text);
                if (prefixNs != "+" && !IsString && s.Contains("http"))
                {
                    stringReadableCache = null;
                    return false;
                    ///localValue = null;
                }
                return true;
            }

            public Atom(object head)
            {
                if (head is INode)
                {
                    if (Init((INode)head))
                    {
                        AddToAtomTable();
                        hash_code = name.GetHashCode();
                    }
                    return;
                }
                _objRef = head;
                Warn("unknown atom class: " + head.GetType());
            }
            public bool Init(INode head)
            {
                bool ret = Init0(head);
                if (!SanityCheck()) return false;
                if (ret) return true;
                return false;
            }
            public bool Init0(INode head)
            {
                Functor0Function = INodeToObject;
                _objRef = head;
                _objRef = ToValueNode((INode) head);
                return ToRoundTripConstructor(head, out prefixNs, out localValue, out quoted);
            }

            static public bool ToRoundTripConstructor(INode head, out string prefixNs,out string localValue,out string quoted)
            {
                if (head is IGraphLiteralNode)
                {
                    Warn("is this a typed literal?! " + head);
                }
                if (head is ILiteralNode)
                {
                    ILiteralNode literalNode = (ILiteralNode) head;
                    localValue = literalNode.Value;
                    quoted = SYNTAX_DoubleQuotes;
                    string language = literalNode.Language;
                    Uri dataType = literalNode.DataType;
                    if (!string.IsNullOrEmpty(language) && dataType!=null)
                    {
                        Warn("is this a plain or typed literal?! " + head);
                    }
                    // special case XmlString
                    if (dataType != null)
                    {
                        if (XmlString == dataType.AbsoluteUri)
                        {
                            if (string.IsNullOrEmpty(language))
                            {
                                prefixNs = XmlString;
                                quoted = SYNTAX_DoubleQuotes;
                                return true;
                            }
                        }
                    }
                    prefixNs = "";
                    if (!string.IsNullOrEmpty(language))
                    {
                        quoted = SYNTAX_DoubleQuotes;
                        prefixNs = language;
                        // Warn("Not capturing language");
                    }
                    else
                    {
                        if (dataType != null)
                        {
                            quoted = SYNTAX_LiteralDataType;
                            prefixNs = dataType.AbsoluteUri;
                        }
                    }
                    return true;
                }
                if (head is IBlankNode)
                {
                    prefixNs = "_";
                    localValue = ((IBlankNode) head).InternalID;
                    quoted = SYNTAX_UriQuotes;
                    return true;
                }
                if (head is IVariableNode)
                {
                    prefixNs = "?";
                    localValue = ((IVariableNode) head).VariableName;
                    quoted = SYNTAX_NoQuotes;
                    return false;
                }
                if (head is IUriNode)
                {
                    quoted = SYNTAX_UriQuotes;
                    IUriNode urin = (IUriNode) head;
                    Uri uri = urin.Uri;
                    string path = uri.AbsoluteUri;
                    string ouri, opref;
                    bool devolved = GraphWithDef.DevolveURI(rdfDefNS, path, out ouri, out opref,
                                                            out localValue, true, false);
                    if (!devolved)
                    {
                        Warn("cant convert to CNAME");
                    }
                    prefixNs = ouri ?? opref;
                    if (false && string.IsNullOrEmpty(localValue))
                    {
                        localValue = prefixNs;
                        prefixNs = "+";
                    }
                    return true;
                }
                throw ErrorBadOp("cant convert " + head);
                prefixNs = null;
                localValue = null;
                quoted = null;
                return false;
            }

            private static bool MustBeQuoted(string localAname)
            {
                if (Regex.Match(localAname, @"^([a-z][a-zA-Z0-9_]*)$").Success)
                {
                    return false;
                }
                if (Regex.Match(localAname, @"^([-]?[0-9]*[\.]?[0-9]+[0-9]*)$").Success)
                {
                    return false;
                }
                return true;
            }

            public override void print(Action<string> w)
            {
                w(this.ToSource(tl_console_language));
            }

            public bool IsLiteral
            {
                get
                {
                    if (quoted == SYNTAX_DoubleQuotes || quoted == SYNTAX_LiteralDataType) return true;
                    return objRef is ILiteralNode;
                }
            }
            public bool IsNode
            {
                get { return objRef is INode; }
            }
            public bool IsValueNode
            {
                get { return objRef is IValuedNode; }
            }
            public bool IsString
            {
                get
                {
                    if (prefixNs == XmlString) return true;
                    var objRef = this._objRef;
                    if (objRef == null)
                    {
                        return false;
                    }
                    return objRef is String || (!(objRef is IUriNode) && !(objRef is NumericNode));
                }
            }

            protected static string XmlString = XmlSpecsHelper.XmlSchemaDataTypeString;

            public bool IsUri
            {
                get { return objRef is IUriNode; }
            }
            public string IsReadable
            {
                get { return ToSource(tl_console_language) + " as " + objRef.GetType() + "=" + objRef; }
            }
            public override sealed string ToSource(SourceLanguage language)
            {
                if (language == SourceLanguage.Text)
                {
                    return name;
                }
                if (true || this.stringReadableCache == null)
                {
                    this.stringReadableCache = string.Intern(StringReadable);
                }
                return this.stringReadableCache;
            }

            public bool IsLocalPrefix
            {
                get
                {
                    if (!IsUri) return false;
                    string ns = Namespace;
                    if (ns == ":") return true;
                    if (ns.StartsWith(RoboKindURI)
                        || ns == RoboKindPrefixPrepend || ns == "siprolog:")
                    {
                        return true;
                    }
                    return false;
                }
            }

            public string StringReadable
            {
                get
                {
                    var objRef = this._objRef;
                    ILiteralNode litnode = objRef as ILiteralNode;
                    if (litnode != null) return StringLiteralReadable;

                    string name = this.name;
                    if (name == "nil")
                    {
                        return "[]";
                    }
                    if (IsString)
                    {
                        return q(localValue);
                    }
                    if (prefixNs == ":")
                    {
                        return aq(name);
                    }
                    if (prefixNs == "_")
                    {
                        return urlq("_:" + localValue);
                    }
                    if (prefixNs == "+")
                    {
                        return urlq(localValue);
                    }
                    var LocalName = this.LocalName ?? name;

                    if (IsLocalPrefix)
                    {
                        return aq(LocalName);
                    }
                    if (LocalName == "" && !string.IsNullOrEmpty(prefixNs))
                    {
                        // to return namespace declarations
                        return urlq(prefixNs);
                    }
                    var ns = Namespace ?? prefixNs;
                    bool usedAbsolute;
                    string url = GraphWithDef.CombinePrefix(ns, name, out usedAbsolute);
                    if (objRef is NumericNode) return name;
                    if (url == null)
                    {
                        var node1 = ToValueNode((INode) objRef);
                        var node = node1 as IUriNode;
                        if (node != null)
                        {
                            if (url == null)
                            {
                                url = node.ToString();
                            }
                        }
                        if (node1 is IBlankNode)
                        {
                            url = node1.ToString();
                        }
                    }
                    if (url != null)
                    {
                        if (quoted == null || quoted.Length != 2)
                        {
                            return urlq(url);
                        }
                        return quoted[0] + (url) + quoted[1];
                    }
                    throw ErrorBadOp("Cant find the nodetype on  " + objRef);
                }
            }

            public string StringLiteralReadable
            {
                get
                {
                    {
                        ILiteralNode litnode = objRef as ILiteralNode;
                        // all the below are now Literal Nodes of some type  (we divide into  "strings", numbers and "strings with"^"meaning" and 
                        if (litnode == null)
                        {
                            if (true) return name;
                        }


                        var ivnode = litnode.AsValuedNode();
                        var value = litnode.Value;
                        var dt = litnode.DataType;
                        var lt = litnode.Language;
                        string et = String.Empty;// ivnode.EffectiveType;
                        if (dt != null) et = dt.AbsoluteUri;

                        if (lt == "" && (dt != null && dt.AbsoluteUri == XmlString))
                        {
                            return q(value);
                        }
                        return "'$obj'('$literal'," + q(value) + "," + q(lt) + "," + q(et) + ")";
                    }
                }
            }

            public static string q(string toWrap, string startChar, string endChar)
            {
                return startChar + toWrap.Replace("\\", "\\\\").Replace(endChar, "\\" + endChar) + endChar;
            }

            private static string q(string s)
            {
                return q(s, "\"", "\"");
            }
            private static string aq(string s)
            {
                if (!Atom.MustBeQuoted(s)) return s;
                return q(s, "'", "'");
            }
            internal static string urlq(string s)
            {
                return q(s, "<", ">");
            }

            public static Atom MakeString(string s)
            {
                return MakeLiteral(s, "", "");
            }

            public static Atom MakeLiteral(string s, string langspec, string dataType)
            {
                if (string.IsNullOrEmpty(langspec))
                {
                    if (!string.IsNullOrEmpty(dataType))
                    {
                        return MakeNewAtom(dataType, s, SYNTAX_LiteralDataType);
                    }
                }
                langspec = langspec ?? "";
                return MakeNewAtom(langspec, s, SYNTAX_DoubleQuotes);
            }

            public static Atom FromName(string s)
            {
                return FromSource0(s, SYNTAX_AtomQuotes);
            }

            /// <summary>
            /// Reads the string that came in from the prolog reader and
            ///  then detects what rdf/sparql expresions 
            /// </summary>
            /// <param name="s"></param>
            /// <returns></returns>
            public static Atom FromSource(string s)
            {
                char c0 = s[0];
                int sl = s.Length;
                if (sl == 1) return FromName(s);

                string quoting = MustGuessQuotes;
                if (s == "[]" || s == FUNCTOR_NIL) return PrologNIL;
                //if (s == "." || s == FUNCTOR_CONS) s = "'robokind:cons'";
                INode makeNode = null;
                bool isNumberMaybe = c0 == '+' || c0 == '-' || char.IsDigit(c0);
                if (isNumberMaybe)
                {
                    makeNode = GraphWithDef.CExtracted(rdfDefinations, s);
                    if (makeNode != null)
                    {
                        return MakeNodeAtom(makeNode);
                    }
                    quoting = SYNTAX_NoQuotes;
                }
                return FromSourceReader(s, quoting);
            }

            public static Atom PrologEmptyString = MakeString("");
            static public Atom PrologNIL = MakeNewAtom(NamespaceMapper.RDF, "nil", SYNTAX_UriQuotes);

            private static void Requote(ref string s, ref string quoting)
            {
                if (quoting == SYNTAX_DoubleQuotes)
                {
                    return;
                }
                char c0 = s[0];
                if (!char.IsSymbol(c0)) return;
                int sl = s.Length;
                char cL = s[sl - 1];
                if (sl == 1) cL = '\0';
                if (c0 == '"' && cL == c0)
                {
                    s = s.Substring(1, sl - 2);
                    quoting = SYNTAX_DoubleQuotes;
                }
                if (c0 == '\'' && cL == c0)
                {
                    s = s.Substring(1, sl - 2);
                    quoting = "" + c0 + cL;
                }
                if (c0 == '<' && cL == '>')
                {
                    s = s.Substring(1, sl - 2);
                    quoting = "" + c0 + cL;
                }
                if (c0 == '{' && cL == '}')
                {
                    // Warn("Tried to make atom from: {0}", s);
                    //return null;
                    s = s.Substring(1, sl - 2);
                    quoting = "<>";// +c0 + cL;
                }
            }

            public static Atom FromSourceReader(string s, string syntaxQuoting)
            {
                var prev = tl_console_language;
                try
                {
                    Requote(ref s, ref syntaxQuoting);
                    return FromSource0(s, syntaxQuoting);
                }
                finally
                {
                    tl_console_language = prev;
                }
            }
            public static Atom FromSource0(string s, string quoting)
            {
                if (quoting == MustGuessQuotes)
                {
                    Requote(ref s, ref quoting);
                }
                if (s == "[]" || s == FUNCTOR_NIL)
                {
                    return PrologNIL;
                }
                if (s == null)
                {
                    Warn("FromSource read NULL");
                    return null;
                }

                bool uriIsh = s.Contains(":") || s.Contains(":/") || s.Contains("#");
                NodeType nodeType = uriIsh ? NodeType.Uri : NodeType.Blank;
                if (quoting == MustGuessQuotes)
                {
                    if (uriIsh)
                    {
                        quoting = SYNTAX_UriQuotes;
                    }
                }
                if (s == null)
                {
                    Warn("FromSource read NULL");
                    return null;
                }
                if (s == "")
                {
                    if (quoting == SYNTAX_DoubleQuotes)
                    {
                        return PrologEmptyString;
                    }
                    Warn("FromSource read EOF");
                    return null;
                }
                if (quoting == SYNTAX_DoubleQuotes)
                {
                    return MakeString(s);
                }
                if (quoting == null && s[0] == '$')
                {
                    nodeType = NodeType.Uri;
                    quoting = SYNTAX_AtomQuotes;
                }
                return MakeAtom(null, s, quoting, nodeType);
            }

            public static Atom MakeAtom(string prefix, string s, string quoting, NodeType nodeType)
            {
                if (s == prefix)
                {
                    prefix = "+";
                }
                else if (prefix == null)
                {
                    return GraphWithDef.ResolveC<Atom>(rdfDefNS, s,
                                                       (a, b, c) => MakeAtom(a, b, quoting, c));
                }
                if (nodeType == NodeType.Uri && prefix.Length > 1)
                {
                    string cc = prefix + s;

                    bool protop = cc.Contains(":/");
                    var lastCharOk = "#/?=";
                    if (!protop) lastCharOk = ":";
                    string prefixHas = null;
                    if (!prefix.Contains(":"))
                    {
                        char lc = prefix[prefix.Length - 1];
                        if (lastCharOk.IndexOf(lc) == -1)
                        {
                            prefix = prefix + lastCharOk[0];
                            prefixHas = "" + lc;
                        }
                    }
                    if (prefixHas != null && s.StartsWith("" + prefixHas))
                    {
                        s = s.Substring(prefixHas.Length);
                    }
                }

                switch (quoting)
                {
                    case SYNTAX_LiteralDataType:
                        {
                            return MakeLiteral(s, "", prefix);
                        }
                    case SYNTAX_DoubleQuotes:
                        {
                            if (string.IsNullOrEmpty(prefix))
                            {
                                return MakeString(s);
                            }
                            return MakeLiteral(s, prefix, "");
                        }
                    case SYNTAX_NoQuotes:
                    case MustGuessQuotes:
                        {
                            break;
                            if (IsVarName(s))
                            {
                                return MakeNodeAtom(rdfDefinations.CreateVariableNode(s));
                            }
                            if (prefix == null)
                            {
                                return MakeNodeAtomFixme(GraphWithDef.CExtracted(rdfDefinations, s));
                            }
                            return MakeNodeAtomFixme(GraphWithDef.C(rdfDefinations, GraphWithDef.MakeQNameOrUri(s, prefix)));
                        }
                    case SYNTAX_UriQuotes:
                        {
                            nodeType = NodeType.Uri;
                            break;
                            return MakeNodeAtomFixme(GraphWithDef.C(rdfDefinations, GraphWithDef.MakeQNameOrUri(s, prefix)));
                        }
                    case SYNTAX_AtomQuotes:
                        {
                            break;
                            return MakeNodeAtomFixme(GraphWithDef.C(rdfDefinations, GraphWithDef.MakeQNameOrUri(s, prefix)));
                        }
                    default:
                        throw ErrorBadOp(s + " " + quoting);
                }
                return MakeNewAtom(prefix, s, quoting);
            }
            
            static public Atom MakeNewAtom(string prefix, string p, string quoting)
            {
                if (prefix == null)
                {
                    return GraphWithDef.ResolveC<Atom>(rdfDefNS, p,
                                                       (a, b, c) => MakeAtom(a, b, quoting, c));
                }
                string atomKey = MakeAtomKey(prefix, p, quoting);

                if (prefix == "+" || prefix == "+:")
                {
                    Warn("bad prefix! " + atomKey);
                }
                if (prefix != ":" && prefix.EndsWith(":"))
                {
                    if (prefix.Length < 3)
                    {
                        Warn("bad prefix! " + atomKey);
                    }
                    prefix = prefix.TrimEnd(':');
                    atomKey = MakeAtomKey(prefix, p, quoting);
                }
                
                lock (AtomTable)
                {
                    Atom atom = null;
                    if (!AtomTable.TryGetValue(atomKey, out atom))
                    {
                        atom = new Atom(prefix, p, quoting);
                        return atom;
                    }
                    return atom;
                }
            }

            public static string MakeAtomKey(string prefix, string s, string quoting0)
            {
                string quoting;
                switch (quoting0)
                {
                    case SYNTAX_LiteralDataType:
                    case SYNTAX_DoubleQuotes:
                        quoting = quoting0;
                        break;
                    case SYNTAX_NoQuotes:
                    case MustGuessQuotes:
                    case SYNTAX_UriQuotes:
                    case SYNTAX_AtomQuotes:
                        quoting = SYNTAX_NoQuotes;
                        break;
                    default:
                        throw ErrorBadOp(s + " " + quoting0);
                }
                return "" + prefix + "^" + s + "^" + quoting + "^";
            }


            public static INode MakeNodeInside(string prefix, string s, string quoting)
            {
                switch (quoting)
                {
                    case SYNTAX_LiteralDataType:
                        {
                            if (!string.IsNullOrEmpty(prefix))
                            {
                                return rdfDefinations.CreateLiteralNode(s, UriFactory.Create(prefix));
                            }
                            Warn("Creating typed literal of unknown type");
                            return rdfDefinations.CreateLiteralNode(s, (Uri)null);
                        }
                    case SYNTAX_DoubleQuotes:
                        {
                            if (string.IsNullOrEmpty(prefix))
                            {
                                return rdfDefinations.CreateLiteralNode(s);
                            }
                            return rdfDefinations.CreateLiteralNode(s, prefix);
                        }
                    case SYNTAX_NoQuotes:
                    case MustGuessQuotes:
                        {
                            if (IsVarName(s))
                            {
                                return rdfDefinations.CreateVariableNode(s);
                            }
                            return GraphWithDef.CExtracted(rdfDefinations, s);
                        }
                    case SYNTAX_UriQuotes:
                        {
                            return GraphWithDef.C(rdfDefinations, GraphWithDef.MakeQNameOrUri(s, prefix));
                        }
                    case SYNTAX_AtomQuotes:
                        {
                            return GraphWithDef.C(rdfDefinations, GraphWithDef.MakeQNameOrUri(s, prefix));
                        }
                    default:
                        return GetValuedNode(s);
                        throw ErrorBadOp(s + " " + quoting);
                }
            }
            public static Atom MakeNodeAtomFixme(INode makeNode)
            {
                return MakeNodeAtom(makeNode);
            }
            public static Atom MakeNodeAtom(INode makeNode)
            {
                string prefixNs, localValue, quoted;
                if(!ToRoundTripConstructor(makeNode, out prefixNs, out localValue, out quoted))
                {
                    Warn("Bad atom??! " + makeNode);
                }
                string atomKey = MakeAtomKey(prefixNs, localValue, quoted);
                lock (AtomTable)
                {
                    Atom atom = null;
                    if (!AtomTable.TryGetValue(atomKey, out atom))
                    {
                        atom = new Atom(makeNode);
                        return atom;
                    }
                    if (!makeNode.Equals(atom.objRef))
                    {
                        Warn("Bad atom key??! " + atomKey);
                    }
                    return atom;
                }
            }

            override public string AsString()
            {
                return AsValuedNode().AsString();
            }

            public bool Unify(IAtomic atomic)
            {
                if (ReferenceEquals(null, atomic))
                {
                    return false;
                }
                if (Functor0.Equals(atomic.Functor0))
                {
                    return true;
                }
                return false;
            }

            public override double AsDouble()
            {
                return AsValuedNode().AsDouble();
            }

            public IValuedNode AsValuedNode()
            {
                var node = AsRDFNode();
                if (node == null)
                {
                    Warn("not a valuednode " + this);
                }
                return node.AsValuedNode();
            }

            public int CompareTo(IAtomic atomic)
            {
                return AsRDFNode().CompareTo(atomic.AsRDFNode());
            }

            public INode AsRDFNode()
            {
                var objRef = this.objRef;
                if (objRef is INode) return ((INode)objRef);
                Warn("Cant make an RDF node");
                return null;
            }

            /// <summary>
            /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
            /// </summary>
            /// <returns>
            /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
            /// </returns>
            /// <param name="obj">The <see cref="T:System.Object"/> to compare with the current <see cref="T:System.Object"/>. 
            ///                 </param><exception cref="T:System.NullReferenceException">The <paramref name="obj"/> parameter is null.
            ///                 </exception><filterpriority>2</filterpriority>
            public override bool Equals(Part obj)
            {
                return Equals(obj as IAtomic);
            }
            public bool Equals(IAtomic other)
            {
                if (ReferenceEquals(null, other)) return false;
                if (ReferenceEquals(this, other)) return true;
                if (this.name != ((Atom)other).name)
                {
                    return false;
                }
                if (this.prefixNs != ((Atom)other).prefixNs)
                {
                    Warn("Non matching prefixes {0}!={1}", this.prefixNs, ((Atom)other).prefixNs);
                    return false;
                }
                if (IsNode && other.IsNode)
                {
                    INode mynode = AsRDFNode();
                    INode othernode = other.AsRDFNode();
                    if (mynode.Equals(othernode))
                    {
                        return true;
                    }
                    return false;
                }
                return Equals(other.Functor0, Functor0);
            }


            public override int GetPlHashCode()
            {
                if (!hash_code.HasValue)
                {
                    hash_code = (objRef != null ? objRef.GetHashCode() : 0);
                }
                return hash_code.Value;
            }
        }
    }
}

