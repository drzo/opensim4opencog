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
        public class Atom : AtomBase, IAtomic
        {
            public Atom(object head)
                : base(head)
            {
            }

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
        }
        abstract public class AtomBase : Part
        {
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as Atom;
                if (ReferenceEquals(term2, null)) return false;
                return this.Unify(term2);
            }

            public Object objRef;
            string aname;
            string rname;
            public string Namespace
            {
                get
                {
                    if (!IsUri) return null;
                    string localAname;
                    //if (aname != null) return aname;
                    string oprefix, ouri, path = AsValuedNode().AsString();
                    if (!GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out ouri, out oprefix,
                                                            out localAname)) return null;

                    return oprefix;
                }
            }
            public string LocalName
            {
                get
                {
                    if (!IsUri) return null;
                    string localAname = aname;
                    if (localAname != null) return localAname;
                    string s = AsValuedNode().AsString();
                    string oprefix, ouri, path = s;
                    if (!GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out ouri, out oprefix,
                                                 out localAname)) return null;

                    aname = localAname = string.Intern(localAname);
                    return localAname;
                }
            }
            private readonly Func<object, object> Functor0Function;
            public override string name
            {
                get
                {
                    if (aname != null) return aname;
                    var f = "" + Functor0;
                    if (f.Contains("#"))
                    {

                    }
                    aname = string.Intern(f);
                    return aname;
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
                    bool devolved = GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out uri, out prefix,
                                                            out localAname);
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
            public AtomBase(Object head)
            {
                //quoted = quoting;
                //aname = bareAtomName;
                objRef = head;
                if (head is INode)
                {
                    objRef = ToValueNode((INode)head);
                    Functor0Function = INodeToObject;
                    // call once to populate the data
                    if (RdfDeveloperSanityChecks > 1)
                    {
                        var localAname = "" + INodeToObject(objRef);
                    }
                }
                else if (head is string)
                {
                    aname = (string)head;
                    hash_code = aname.GetHashCode();
                }
                else
                {
                    Warn("unknown atom class: " + objRef.GetType());
                }
                string s = ToSource(SourceLanguage.Text);
                if (!IsString && s.Contains("http"))
                {
                    rname = null;
                    aname = null;

                }
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
                get { return objRef is ILiteralNode; }
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
                get { return objRef is String || (!(objRef is IUriNode) && !(objRef is NumericNode)); }
            }
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
                if (true || this.rname == null)
                {
                    this.rname = string.Intern(StringReadable);
                }
                return this.rname;
            }

            public bool IsLocalPrefix
            {
                get
                {
                    var node = objRef as IUriNode;
                    if (node != null)
                    {
                        if (node.ToString().StartsWith(RoboKindURI)) return true;
                    }
                    return false;
                }
            }
            public string StringReadable
            {
                get
                {
                    string name = this.name;
                    if (name == "nil")
                    {
                        return "[]";
                    }
                    if (objRef is INode)
                    {
                        var node1 = ToValueNode((INode)this.objRef);
                        if (node1 is NumericNode) return name;
                        var node = node1 as IUriNode;
                        if (node != null)
                        {
                            string url = null;
                            var ns = Namespace;
                            if (IsLocalPrefix || ns == RoboKindPrefix || ns == "siprolog")
                            {
                                return aq(LocalName);
                            }
                            if (ns != null)
                            {
                                url = ns + ":" + LocalName;
                            }
                            if (url == null)
                            {
                                url = node.ToString();
                            }
                            if (quoted == null || quoted.Length != 2)
                            {
                                return "<" + url + ">";
                            }
                            return quoted[0] + (url) + quoted[1];
                        }
                        if (node1 is IBlankNode)
                        {
                            return "<" + node1 + ">";
                        }
                        ILiteralNode litnode = node1 as ILiteralNode;
                        // all the below are now Literal Nodes of some type  (we divide into  "strings", numbers and "strings with"^"meaning" and 
                        if (litnode == null)
                        {
                            throw ErrorBadOp("Cant find the nodetype on  " + node);
                        }


                        var ivnode = litnode.AsValuedNode();
                        var value = litnode.Value;
                        var dt = litnode.DataType;
                        var lt = litnode.Language;
                        string et = String.Empty;// ivnode.EffectiveType;
                        if (dt != null) et = dt.AbsoluteUri;

                        if (lt == "" && (dt != null && dt.AbsoluteUri == XmlSpecsHelper.XmlSchemaDataTypeString))
                        {
                            return q(value);
                        }
                        return "'$obj'('$literal'," + q(value) + "," + q(lt) + "," + q(et) + ")";
                    }
                    if (quoted == null || quoted == "")
                    {
                        char fc = name[0];
                        if (char.IsLetter(fc) && char.IsLower(fc))
                        {
                            if (name.IndexOfAny("'\"?!@#$%^&*(){}:;/,<>".ToCharArray()) == -1 && !(name.IndexOfAny("+-".ToCharArray()) > 0))
                            {
                                if (MustBeQuoted(name))
                                {
                                    return q(name);
                                }
                                return name;
                            }
                        }
                        return q(name);
                        quoted = MustBeQuoted(name) ? SYNTAX_DoubleQuotes : "";
                    }
                    if (quoted == "")
                    {
                        return name;
                    }
                    return quoted[0] + name + quoted[1];
                }
            }

            private static string q(string s)
            {
                return "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
            }
            private static string aq(string s)
            {
                if (!Atom.MustBeQuoted(s)) return s;
                return "'" + s.Replace("\\", "\\\\").Replace("'", "\\'") + "'";
            }

            public static Atom MakeString(string s)
            {
                return MakeLiteral(s, "", XmlSpecsHelper.XmlSchemaDataTypeString);
            }

            public static Atom MakeLiteral(string s, string langspec, string dataType)
            {
                string syntax = SYNTAX_LiteralDataType;
                if (string.IsNullOrEmpty(dataType))
                {
                    syntax = SYNTAX_DoubleQuotes;
                    return MakeNodeAtom(rdfDefinations.CreateLiteralNode(s, langspec ?? ""));
                }
                return MakeNodeAtom(rdfDefinations.CreateLiteralNode(s, UriFactory.Create(dataType)));
            }

            public static Atom MakeUri(string s)
            {
                return MakeNodeAtom(rdfDefinations.CreateUriNode(UriFactory.Create(s)));
            }
            public static Atom FromName(string s)
            {
                return MakeNodeAtom(MakeNode(s, SYNTAX_AtomQuotes));
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
                if (s == "[]" || s == FUNCTOR_NIL) s = "'rdf:nil'";
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
                Requote(ref s, ref quoting);
                return FromSourceReader(s, quoting);
            }

            private static void Requote(ref string s, ref string quoting)
            {
                char c0 = s[0];
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
                    return FromSource0(s, syntaxQuoting);
                }
                finally
                {
                    tl_console_language = prev;
                }
            }
            public static Atom FromSource0(string s, string quoting)
            {
                if (s == "[]" || s == FUNCTOR_NIL)
                {
                    s = "rdf:nil";
                }
                if (quoting == MustGuessQuotes)
                {
                    if (s.Contains(":") || s.Contains("/"))
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
                        return MakeString(s);
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
                    quoting = SYNTAX_AtomQuotes;
                }
                return MakeNodeAtom(MakeNode(s, quoting));
            }

            public static INode MakeNode(string s, string quoting)
            {
                switch (quoting)
                {
                    case SYNTAX_DoubleQuotes:
                        {
                            return Atom.MakeString(s).AsRDFNode();
                        }
                    case "":
                    case null:
                        {
                            if (IsVarName(s))
                            {
                                return rdfDefinations.CreateVariableNode(s);
                            }
                            return GraphWithDef.CExtracted(rdfDefinations, s);
                        }
                    case "{}":
                    case "<>":
                    case "''":
                        {
                            return GraphWithDef.C(rdfDefinations, s);
                        }
                    default:
                        return GetValuedNode(s);
                        throw ErrorBadOp(s + " " + quoting);
                }
            }

            public static Atom MakeNodeAtom(INode makeNode)
            {
                string s = makeNode.ToString();
                lock (AtomTable)
                {
                    //string atomKey = quoting.ToString() + s;
                    Atom atom = null;
                    if (true ||
                        (!AtomTable.TryGetValue(makeNode, out atom) ||
                         (makeNode != null && (atom.objRef != null && atom.objRef != makeNode))))
                    {
                        atom = new Atom(makeNode);
                        //AtomTable[atomKey] = atom;
                        return atom;
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

