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
using VDS.RDF.Query.Expressions;
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
        public const string basePrefixDefault = "#";
        public const bool BnodeUseGraph = true;
        public const string BnodePrefixGraph = "_:";
        public class Atom : Part, IAtomic
        {
            public override Term AsTerm()
            {
                if (IsLiteral)
                {
                    ILiteralNode litnode = objRef as ILiteralNode;
                    if (litnode != null)
                    {
                        var dt = litnode.DataType;
                        var lt = litnode.Language;
                        string et = String.Empty; // ivnode.EffectiveType;
                        if (dt != null) et = dt.AbsoluteUri;
                        return
                            MakeTerm("$obj",
                                     FromName("$literal"), MakeString(litnode.Value), MakeString(lt), MakeString(et));
                    }
                }
                // fname/0
                return new Term(fname, false, new PartListImpl());
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
                        var mn = MakeNodeInside(prefixNs, localValue, quoted);
                        if (RdfDeveloperSanityChecks > 2) return mn;
                        _objRef = mn;
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
                if (ns.Contains(ProtocolSep)) return ns;
                if (ns.EndsWith(":"))
                {
                    ns = ns.Substring(0, ns.Length - 1);
                }
                if (ns == basePrefixDefault) return ns;
                if (rdfDefNS.HasNamespace(ns))
                {
                    var uri =  rdfDefNS.GetNamespaceUri(ns).AbsoluteUri;
                    if (ns == basePrefixDefault || uri == basePrefixDefault)
                    {
                        return "";
                    }
                    return uri;
                }
                return ns;
            }
            static string GetNamespacePrefix(string ns)
            {
                if (ns.Contains(ProtocolSep))
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
                    if (ns == null)
                    {
                        ns = NamespaceX();
                    }
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
            public string NamespaceOLD
            {
                get
                {
                    return NamespaceX();
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
                                             out localAname, true, false))
                {
                    return null;
                }
                var ns = oprefix ?? ouri ?? prefixNs;
                oprefix = String.Intern(ns);
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
                    localAname = String.Intern(localAname);
                    return localAname;
                }
            }
            private Func<object, object> Functor0Function;
            public string name
            {
                get
                {                    
                    if (IsString)
                    {
                        return localValue;
                    }
                    if (IsNumber)
                    {
                        return localValue;
                    }
                    var localName = LocalName ?? this.localValue;
                    if (String.IsNullOrEmpty(localName))
                    {
                        localName = NamespaceUri;
                    }
                    if (!String.IsNullOrEmpty(localName)) return localName;
                    var f = "" + Functor0;
                    var fi = String.Intern(f);
                    return fi;
                }
            }
            public override string fname
            {
                get
                {
                    return name;
                }
            }
            public override string fvname
            {
                get
                {
                    return fname;
                }
            }
            public override string vname
            {
                get
                {
                    return fvname;
                }
            }
            public override object Functor0
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
                    bool noaname = String.IsNullOrEmpty(localAname);
                    if (devolved && !noaname)
                    {
                        if (String.IsNullOrEmpty(prefix))
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
            public Atom(string prefix, string localName, string quoteType)
            {
                if (String.IsNullOrEmpty(localName) && String.IsNullOrEmpty(prefix) && quoteType != SYNTAX_DoubleQuotes)
                {
                    localName = prefix;
                    prefix = "";
                }
                prefixNs = prefix;
                localValue = localName;
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
            public string AtomKey
            {
                get
                {
                    return MakeAtomKey(prefixNs, localValue, quoted);
                }
            }
            private void AddToAtomTable()
            {
                string key = AtomKey;
                AtomTable[key] = this;
            }

            private bool SanityCheck()
            {
                if (_objRef is IBlankNode)
                {
                    return true;
                }
                if (RdfDeveloperSanityChecks <= 1) return true;
                var name = this.name;
                FirstUse<Atom> pes = PrologEmptyString;
                if (!pes.HasValue) return true;
                if (ReferenceEquals(this, pes.Value)) return true;
                if (prefixNs == "+")
                {
                    Warn("OLD +ish Atom: " + IsReadable);
                }
                if (String.IsNullOrEmpty(prefixNs))
                {
                    if (!IsLiteral) Warn("OLD +ish Atom: " + IsReadable);
                }
                do
                {
                    if (String.IsNullOrEmpty(localValue) //|| string.IsNullOrEmpty(quoted)
                        || String.IsNullOrEmpty(prefixNs)
                        || String.IsNullOrEmpty(name)
                        || String.IsNullOrEmpty(StringReadable))
                    {
                        if (IsLiteral)
                        {
                            if (String.IsNullOrEmpty(prefixNs)) break;
                        }
                        if (prefixNs == NamespaceUri)
                        {
                            break;
                        }
                        if (prefixNs == Namespace)
                        {
                            break;
                        }
                        Warn("Imporoper Atom: " + IsReadable);
                        return false;
                    }
                    break;
                } while (true);
                var localAname = "" + INodeToObject(objRef);
                string s = ToSource(SourceLanguage.Text);
                if (prefixNs != "+" && !IsString && !String.IsNullOrEmpty(localValue) && s.Contains("http"))
                {
                    stringReadableCache = null;
                    Warn("Text represention is lossy! " + s);
                    return false;
                    ///localValue = null;
                }
                stringReadableCache = null;
                return true;
            }

            protected Atom(object head, bool typeCheck)
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
                if (typeCheck) Warn("unknown atom class: " + head.GetType());
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
                if (!ToRoundTripConstructor(head, out prefixNs, out localValue, out quoted))
                {
                    return false;
                }
                if (RdfDeveloperSanityChecks < 2) return true;
                SIProlog.checkNode(head);
                var mn = MakeNodeInside_0(prefixNs, localValue, quoted);
                if (head.Equals(mn))
                {
                    return true;
                }
                mn = MakeNodeInside(prefixNs, localValue, quoted);
                if (head.Equals(mn.CopyWNode(head.Graph))) return true;
                throw ErrorBadOp("cant intern " + head);
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
                    if (!String.IsNullOrEmpty(language) && dataType!=null)
                    {
                        Warn("is this a plain or typed literal?! " + head);
                    }
                    // special case XmlString
                    if (dataType != null)
                    {
                        if (XmlString == dataType.AbsoluteUri)
                        {
                            if (String.IsNullOrEmpty(language))
                            {
                                prefixNs = XmlString;
                                quoted = SYNTAX_DoubleQuotes;
                                return true;
                            }
                        }
                    }
                    prefixNs = "";
                    if (!String.IsNullOrEmpty(language))
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
                    quoted = SYNTAX_UriQuotes;
                    if (BnodeUseGraph)
                    {
                        localValue = BnodePrefixGraph + ((IBlankNode)head).InternalID;
                        Uri uri = head.Origin();
                        if (uri == null)
                        {
                            Warn("nUll URI on " + head);
                            prefixNs = "#";
                        }
                        else
                        {
                            prefixNs = uri.AbsoluteUri;
                        }
                        return false;
                    }
                    else
                    {
                        prefixNs = "_";
                        localValue = ((IBlankNode) head).InternalID;
                    }
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
                        GraphWithDef.DevolveURI(rdfDefNS, path, out ouri, out opref,
                                                            out localValue, true, false);
                    }
                    prefixNs = ouri ?? opref;
                    if (false && String.IsNullOrEmpty(localValue))
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
                    if (quoted == SYNTAX_DoubleQuotes) return true;
                    if (prefixNs == XmlString) return true;
                    var objRef = this._objRef;
                    if (objRef == null)
                    {
                        return false;
                    }
                    return objRef is String || (!(objRef is IUriNode) && !(objRef is NumericNode));
                }
            }
            public bool IsNumber
            {
                get
                {
                    var ivnode = AsValuedNode();             
                    if (ivnode != null) return (ivnode.NumericType != SparqlNumericType.NaN);
                    return false;
                }
            }

            protected static string XmlString = XmlSpecsHelper.XmlSchemaDataTypeString;

            public bool IsUri
            {
                get { return objRef is IUriNode; }
            }
            public string IsReadable
            {
                get {
                    return ToSource(tl_console_language) + " as " + objRef.GetType() + "=" + objRef + "=" +
                           StringReadable; }
            }
            public override string Text
            {
                get
                {
                    return name;
                }
            }
            public override sealed string ToSource(SourceLanguage language)
            {
                if (language == SourceLanguage.Text)
                {
                    return name;
                }
                language = language.Inner();
                var nf = language.NodeFormatter;
                if (nf != null)
                {
                    return AsRDFNode().ToString(nf);
                }
                if (language != SourceLanguage.Prolog)
                {
                    object o = Functor0;
                    if (o == null) return null;
                    return "" + Functor0;
                }
                if (this.stringReadableCache == null)
                {
                    if (RdfDeveloperSanityChecks < 2)
                    {
                        this.stringReadableCache = String.Intern(StringReadable);
                    }
                    else
                    {
                        return StringReadable;
                    }
                }
                return this.stringReadableCache;
            }

            public bool IsLocalPrefix
            {
                get
                {
                    if (!IsUri) return false;
                    if (prefixNs == basePrefixDefault) return true;
                    string ns = NamespaceOLD;
                    if (GetBasePrefix(ns) == basePrefixDefault)
                    {
                        return true;
                    }
                    return false;
                }
            }

            public override string StringReadable
            {
                get
               {
                   return ToStringReadable(false, null);
               }
            }
            public string ToStringReadable(bool fullURI, string localBase)
            {                
                {
                    var objRef = this.objRef;
                    ILiteralNode litnode = objRef as ILiteralNode;
                    if (objRef is IBlankNode)
                    {
                        return ToReadbleBNode((IBlankNode)objRef, localBase);
                    }
                    if (litnode != null) return StringLiteralReadable;
                    string name = this.name;
                    if (objRef is NumericNode) return name;
                    var prefixNs = this.prefixNs;
                    var localValue = this.localValue;
                    if (IsString)
                    {
                        return q(localValue);
                    }
                    if (!IsUri)
                    {
                        var nodeInside = MakeNodeInside(prefixNs, localValue, quoted); 
                        Warn("Non URI");
                    }
                    if (name == "nil")
                    {
                        return "[]";
                    }
                    if (String.IsNullOrEmpty(prefixNs))
                    {
                        return aq(name);
                    }
                    if (prefixNs == basePrefixDefault || IsLocalPrefix)
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
                    if (LocalName == "" && !String.IsNullOrEmpty(prefixNs))
                    {
                        // to return namespace declarations
                        return urlq(prefixNs);
                    }
                    var ns = (fullURI ? NamespaceUri : Namespace) ?? prefixNs;
                    bool usedAbsolute;
                    string url = GraphWithDef.CombinePrefix(ns, name, out usedAbsolute);
                    if (fullURI && !usedAbsolute)
                    {
                        Warn("Could not resolve full namespace");
                    }
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

            private string ToReadbleBNode(IBlankNode iBlankNode, string localBase)
            {
                string p = iBlankNode.GraphUri.AbsoluteUri + iBlankNode;
                string s = urlq(NamespaceUri + name);
                return s;
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
                        // numbers
                        if (ivnode != null && ivnode.NumericType != SparqlNumericType.NaN)
                        {
                            return value;
                        }
                        var dt = litnode.DataType;
                        var lt = litnode.Language;
                        string et = String.Empty;// ivnode.EffectiveType;
                        if (dt != null) et = dt.AbsoluteUri;

                        //if (lt == "" && (dt != null && et == XmlString))
                        if (lt == "" && lt == "" && quoted == SYNTAX_DoubleQuotes)
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
            public static string aq(string s)
            {
                if (!MustBeQuoted(s)) return s;
                return q(s, "'", "'");
            }
            internal static string urlq(string s)
            {
                return q(s, "<", ">");
            }

            public static Atom MakeString(string s)
            {
                // dmiles: for now less stack than MakeLiteral
                if (s == "") return PrologEmptyString;
                return MakeNewAtom("", s, SYNTAX_DoubleQuotes);
                // return MakeLiteral(s, "", "");
            }

            public static Atom MakeLiteral(string s, string langspec, string dataType)
            {
                if (String.IsNullOrEmpty(langspec))
                {
                    if (!String.IsNullOrEmpty(dataType))
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
                return FromSourceReader(s, MustGuessQuotes);
            }

            public static FirstUse<Atom> PrologEmptyString = (Func<Atom>)(() => MakeNewAtom("", "", SYNTAX_DoubleQuotes));

            public static FirstUse<Atom> PrologNIL =
                (Func<Atom>) (() => MakeNewAtom(NamespaceMapper.RDF, "nil", SYNTAX_UriQuotes));

            private static void Requote(ref string s, ref string quoting)
            {
                if (quoting == SYNTAX_DoubleQuotes)
                {
                    return;
                }
                char c0 = s[0];
                if (!Char.IsSymbol(c0)) return;
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
                if (s == null)
                {
                    Warn("FromSource read NULL");
                    return null;
                }
                int slen = s.Length;

                if (quoting == MustGuessQuotes)
                {
                    Requote(ref s, ref quoting);
                }
                if (quoting == SYNTAX_DoubleQuotes)
                {
                    if (slen == 0) return PrologEmptyString;
                    return MakeString(s);
                }
                bool slengt1 = slen > 1;
                if (slengt1 && (s == "[]" || s == FUNCTOR_NIL))
                {
                    return PrologNIL;
                }
                bool uriIsh = slengt1 && (s.Contains(":") || s.Contains(ProtocolSep) || s.Contains("#"));
                NodeType nodeType = uriIsh ? NodeType.Uri : NodeType.Blank;
                if (uriIsh)
                {
                    if (quoting == MustGuessQuotes)
                    {
                        quoting = SYNTAX_UriQuotes;
                    }
                }
                else if (quoting == MustGuessQuotes || quoting == SYNTAX_NoQuotes)
                {
                    //if (s == "." || s == FUNCTOR_CONS) s = "'robokind:cons'";
                    char c0 = s[0];
                    bool isNumberMaybe = (slengt1 && (c0 == '+' || c0 == '-')) || Char.IsDigit(c0);
                    if (isNumberMaybe)
                    {
                        bool decm = slengt1 && s.Contains(".");
                        long intv;
                        if (!decm && Int64.TryParse(s, out intv))
                        {
                            return MakeNodeAtom(new LongNode(rdfDefinations, intv));
                        }
                        double dbl;
                        if (decm && Double.TryParse(s, out dbl))
                        {
                            return MakeNodeAtom(new DoubleNode(rdfDefinations, dbl));
                        }
                        quoting = SYNTAX_NoQuotes;
                    }
                }
                if (quoting == MustGuessQuotes && s[0] == '$')
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
                    return GraphWithDef.ResolveC(rdfDefNS, s,
                                                       (a, b, c) => MakeAtom(a, b, quoting, c));
                }
                if (nodeType == NodeType.Uri && prefix.Length > 1)
                {
                    string cc = prefix + s;

                    bool protop = cc.Contains(ProtocolSep);
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
                            if (String.IsNullOrEmpty(prefix))
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
                    return GraphWithDef.ResolveC(rdfDefNS, p,
                                                       (a, b, c) => MakeAtom(a, b, quoting, c));
                }
                string atomKey = MakeAtomKey(prefix, p, quoting);

                if (prefix == "+" || prefix == "+:")
                {
                    Warn("bad prefix! " + atomKey);
                }
                if (prefix == ":")
                {
                    Warn("bad prefix! " + atomKey);
                }
                else if (prefix.EndsWith(":"))
                {
                    if (prefix.Length < 3)
                    {
                        Warn("bad prefix! " + atomKey);
                    }
                    prefix = prefix.TrimEnd(':');
                    atomKey = MakeAtomKey(prefix, p, quoting);
                }
                if (!prefix.EndsWith("#") &&
                    (prefix.Contains("query")
                    || prefix.Contains("sparql")
                    || prefix.Contains("#")))
                {
                    Warn("are we using the base? " + prefix);
                }
                Atom atom = null;
                lock (AtomTable)
                {
                    if (!AtomTable.TryGetValue(atomKey, out atom))
                    {
                        // Atom constructor puts itself in AtomTable
                        atom = new Atom(prefix, p, quoting);
                    }
                }

                return atom;
            }
            private static string GetBasePrefix(string p0)
            {
                if (String.IsNullOrEmpty(p0)) return p0;
                if (p0 == "#" || p0 == basePrefixDefault || p0 == RoboKindPrefix || p0 == "rk")
                    return basePrefixDefault;
                if (p0 == RoboKindURI) return basePrefixDefault;
                if (p0.StartsWith(RoboKindURI) || p0.StartsWith(RoboKindMtURI))
                {
                    return basePrefixDefault;
                }
                return p0;
            }
            private static string GetBaseQuoting(string quoting0)
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
                        quoting = MustGuessQuotes;
                        break;
                }
                return quoting;
            }
            public static string MakeAtomKey(string prefix0, string s, string quoting0)
            {
                string prefix = GetNamespaceUri(prefix0);
                if (String.IsNullOrEmpty(prefix))
                {
                    prefix = prefix0;
                }
                string quoting = GetBaseQuoting(quoting0);
                if (quoting == MustGuessQuotes)
                {
                    {
                        throw ErrorBadOp(s + " " + quoting0);
                    }
                }
                return "" + prefix + "^" + s + "^" + quoting + "^";
            }
            public static INode MakeNodeInside(string prefix, string s, string quoting)
            {
                INode makeNodeInside = MakeNodeInside_0(prefix, s, quoting);
                if (makeNodeInside != null && RdfDeveloperSanityChecks < 2) return makeNodeInside;
                string p0, s0, q0;
                if (!ToRoundTripConstructor(makeNodeInside, out p0, out s0, out q0))
                {
                    Warn("No Round Trip Constructor: " + makeNodeInside);
                }
                if (GetBasePrefix(p0) != GetBasePrefix(prefix) || s != s0 || GetBaseQuoting(quoting) != GetBaseQuoting(q0))
                {
                    MakeNodeInside_0(prefix, s, quoting);
                    Warn("Broken Round Trip Constructor: " + makeNodeInside);
                }
                return makeNodeInside;
            }
            public static INode MakeNodeInside_0(string prefix, string s, string quoting)
            {
                switch (quoting)
                {
                    case SYNTAX_LiteralDataType:
                        {
                            if (!String.IsNullOrEmpty(prefix))
                            {
                                return rdfDefinations.CreateLiteralNode(s, UriFactory.Create(prefix));
                            }
                            Warn("Creating typed literal of unknown type");
                            return rdfDefinations.CreateLiteralNode(s, (Uri)null);
                        }
                    case SYNTAX_DoubleQuotes:
                        {
                            if (String.IsNullOrEmpty(prefix))
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
                            if (s.StartsWith(BnodePrefixGraph))
                            {
                                s = s.Substring(BnodePrefixGraph.Length);
                                var bn =  CreateBlankNode(FindGraph(prefix), s);
                                var bnu = bn.GraphUri;
                                return bn;
                            }
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
                bool deferChecks = RdfDeveloperSanityChecks == 0;
                if (makeNode is IBlankNode)
                {
                    deferChecks = true;
                }
                if (!ToRoundTripConstructor(makeNode, out prefixNs, out localValue, out quoted))
                {
                    if (!deferChecks)
                    {
                        Warn("Bad atom??! " + makeNode);
                    }
                }
                string atomKey = MakeAtomKey(prefixNs, localValue, quoted);
                Atom atom = null;
                lock (AtomTable)
                {
                    if (!AtomTable.TryGetValue(atomKey, out atom))
                    {
                        // Atom constructor puts itself in AtomTable
                        return new Atom(makeNode);
                    }
                }
                if (!makeNode.Equals(atom.objRef))
                {
                    if (deferChecks) return atom;
                    if (RdfDeveloperSanityChecks > 2)
                    {
                        //Warn("Bad atom key??! " + atomKey);
                        makeNode.Equals(atom.objRef);
                    }
                }
                return atom;
            }

            override public string AsString()
            {
                string n = name;
                if (RdfDeveloperSanityChecks == 0) return name;
                string v = AsValuedNode().AsString();
                if (n != v)
                {
                    Warn("A string is misbehaving " + IsReadable);
                }
                return n;
            }

            public bool Unify(IAtomic atomic)
            {
                return SameAs(atomic, false);
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
                return SameAs(obj as IAtomic, true);
            }
            public bool SameAs(IAtomic other0, bool identityMatch)
            {
                Atom other = other0 as Atom;
                if (ReferenceEquals(null, other)) return false;
                if (ReferenceEquals(this, other)) return true;
                if (this.localValue != other.localValue)
                {
                    return false;
                }
                if (GetBasePrefix(this.prefixNs) != GetBasePrefix(((Atom)other).prefixNs))
                {
                    if (this.localValue == "") return false;
                    Warn("Non matching prefixes {0}!={1}", this.prefixNs, ((Atom) other).prefixNs);

                    if (!identityMatch) return true;
                    return false;
                }
                if (identityMatch)
                {
                    if (this.quotedType != other.quotedType)
                    {
                        return false;
                    }
                    return true;
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

            protected string quotedType
            {
                get { return GetBaseQuoting(quoted); }
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

