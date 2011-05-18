using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;

namespace MushDLR223.Utilities
{
    //public delegate string ReduceStar0(string value);
    public interface ITraceable
    {
        bool IsTraced { get; set; }
    }

    public interface ITreeable
    {
        string NameSpace { get; }
        IEnumerable<string> SettingNames(int depth);
    }

    public class StaticXMLUtils
    {
        public static R WithoutTrace<R>(ITraceable itrac, Func<R> func)
        {
            if (!System.Threading.Monitor.TryEnter(itrac,TimeSpan.FromSeconds(2)))
            {
                return func();
            }
            try
            {
                bool prev = itrac.IsTraced;
                if (!prev) return func();
                try
                {
                    itrac.IsTraced = false;
                    return func();

                }
                finally
                {
                    itrac.IsTraced = prev;
                }
            }
            finally
            {
                System.Threading.Monitor.Exit(itrac);
            }
        }

        public static IFormatProvider FormatProvider;
        public static Func<IConvertible, Type, IConvertible> FormatProviderConvertor;

        public static string[] NamesStrings(string name)
        {
            return name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        }

        public static bool NameMatches(XmlNode node, string s)
        {
            if (s.Contains("="))
            {
                var nv = s.Split(new char[] { '=' });
                {
                    if (node.Attributes != null)
                        foreach (XmlAttribute s1 in node.Attributes)
                        {
                            if (SearchStringMatches(nv[0], s1.Name))
                            {
                                if (SearchStringMatches(nv[1], s1.Value))
                                    return true;
                            }
                        }
                }
                return false;
            }
            return SearchStringMatches(s, node.Name) || SearchStringMatches(s, node.LocalName);
        }

        public static bool SearchStringMatches(string pattern, string name)
        {
            if (name == pattern) return true;
            if (pattern == null || name == null) return false;
            name = Trim(name);
            pattern = Trim(pattern);
            int patternLength = pattern.Length;
            if (patternLength == 0) return name.Length == 0;
            //            char pc = pattern[0];
            pattern = ToLower(pattern);
            name = ToLower(name);
            if (name == pattern) return true;
            if (pattern.Contains(","))
            {
                foreach (var c in NamesStrings(name))
                {
                    if (SearchStringMatches(c, name)) return true;
                }
            }
            return !Char.IsLetterOrDigit(pattern[patternLength - 1]) && Regex.IsMatch(name, "^" + pattern + "$");
        }

        public static bool ContainsXml(string s)
        {
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&") && s.Contains(";"))
            {
                return true;
            }
            return false;
        }

        //    #endregion

        public static bool XmlSame(string xml1, string xml2)
        {
            if (xml1 == xml2) return true;
            if (xml1 == null) return String.IsNullOrEmpty(xml2);
            if (xml2 == null) return String.IsNullOrEmpty(xml1);
            xml1 = MakeXmlMatchable(xml1);
            xml2 = MakeXmlMatchable(xml2);
            if (xml1.Length != xml2.Length) return false;
            if (xml1 == xml2) return true;
            if (xml1.ToUpper() == xml2.ToUpper())
            {
                return true;
            }
            return false;
        }

        public static string MakeXmlMatchable(string xml1)
        {
            if (xml1 == null) return null;
            return CleanWhitepaces(xml1.ToLower());
        }

        public static bool IsHtmlTag(string name)
        {
            return " em b underline bold src html head body font pre p div br dd td th tr table frame frameset &ltl &gt; input option select &qt; "
                .Contains(" " + name.ToLower() + " ");
        }

        public static int AttributesCount(XmlNode node, string s)
        {
            if (node.Attributes == null) return 0;
            int i = 0;
            string ss = "," + s + ",";
            foreach (XmlAttribute cn in node.Attributes)
            {
                if (ss.Contains("," + cn.Name + ","))
                {
                    i++;
                }
            }
            return i;
        }

        public static string VisibleRendering(XmlNodeList nodeS, RenderOptions options)
        {
            string sentenceIn = "";
            foreach (XmlNode nodeO in nodeS)
            {
                sentenceIn = sentenceIn + " " + VisibleRendering(nodeO, options);
            }
            return sentenceIn.Trim().Replace("  ", " ");
        }


        public static string VisibleRendering(XmlNode nodeO, RenderOptions options)
        {
            //            if (nodeO.NodeType == XmlNodeType.Comment) return true;
            if (nodeO == null) return null;
            string nodeName = nodeO.Name;//.ToLower();
            if (options.SkipNode(nodeName))
            {
                return " ";
            }
            if (options.FlattenChildren(nodeName))
            {
                return VisibleRendering(nodeO.ChildNodes, options);
            }
            if (nodeO.NodeType == XmlNodeType.Element)
            {
                XmlNodeList nodeOChildNodes = nodeO.ChildNodes;
                if (nodeOChildNodes.Count == 0) return nodeO.OuterXml;
                string bs = nodeStartXML((XmlElement)nodeO);
                string visibleRendering = VisibleRendering(nodeOChildNodes, options);
                if (!String.IsNullOrEmpty(bs))
                {
                    visibleRendering = String.Format("{0}{1}</{2}>", bs, visibleRendering, nodeName);
                }
                return options.CleanText(visibleRendering);
            }
            if (nodeO.NodeType == XmlNodeType.Text) return options.CleanText(TextNodeValue(nodeO));
            return nodeO.OuterXml;
        }

        /*
        public static string ToVisible(XmlNode node)
        {
            string oxml = CleanWhitepaces(node.OuterXml.Replace("<sapi>", " ").Replace("</sapi>", " "));
            if (node.NodeType == XmlNodeType.Attribute)
            {
                return node.Name + "=\"" + node.Value + "\"";
            }
            if (oxml.Trim().Length > 0) return oxml;
            oxml = node.InnerText;
            if (oxml.Trim().Length > 0) return oxml;
            if (node.NodeType == XmlNodeType.Element)
            {
                return node.OuterXml;
            }
            return node.OuterXml;
        }

        public static string ToVisible(string message)
        {
            if (message.Contains("<"))
            {
                try
                {
                    message = CleanWhitepaces(message.Replace("<sapi>", " ").Replace("</sapi>", " "));
                    string s = "";
                    XmlNode v = getNode("<sapi>" + message + "</sapi>");
                    LineInfoElementImpl.unsetReadonly(v);
                    return ToVisible(v);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            return message;
        }
        */

        public static bool IsInnerTextOnly(XmlNode node)
        {
            string s = node.InnerXml.Trim();
            if (s == node.InnerText.Trim())
            {
                if (s.Length > 0) return true;
                return true;
            }
            return false;
        }

        public static string InnerXmlText(XmlNode templateNode)
        {
            string s = InnerXmlText0(templateNode).Trim();
            if (String.IsNullOrEmpty(s))
            {
                return s;
            }
            return ValueText(s);
        }

        public static string ToXmlValue(XmlNode xmlNode)
        {
            string found = ToXMLValueNotOuter(xmlNode);
            if (found != null) return found;
            return xmlNode.OuterXml;
        }
        public static string ToXMLValueNotOuter(XmlNode xmlNode)
        {
            if (xmlNode.NodeType == XmlNodeType.Text)
            {
                var v = ValueText(TextNodeValue(xmlNode, false));
                return v;
            }
            if (xmlNode.NodeType == XmlNodeType.CDATA)
            {
                return ValueText(xmlNode.InnerXml);
            }
            string str = xmlNode.InnerXml;
            bool oneElementChild = xmlNode.NodeType == XmlNodeType.Element && xmlNode.HasChildNodes && xmlNode.ChildNodes.Count == 1;
            if (IsValueSetter(str) && oneElementChild)
            {
                return ValueText(InnerXmlText(xmlNode));
            }
            return null;
        }

        public static bool IsValueSetter(string s)
        {
            return s != null && s.StartsWith(isValueSetStart);
        }

        public static string ValueText(string s)
        {
            if (s.StartsWith(isValueSetStart))
            {
                s = s.Substring(isValueSetSkip);
            }
            return s.TrimStart(isValueSetChars).Trim(" \n\r\t".ToCharArray());
        }
        public static string XmlValueSettable(string stringValue)
        {
            if (stringValue == null) return null;
            stringValue = ValueText(stringValue);
            if (stringValue.Contains("<") && !stringValue.Contains("<!--"))
            {
                writeDebugLine("ERROR: about to write XML into text? " + stringValue);
                return isValueSetStart + stringValue;
            }
            return isValueSetStart + stringValue;
        }

        private static string InnerXmlText0(XmlNode templateNode)
        {
            if (templateNode == null) return "-NULLXML-";
            switch (templateNode.NodeType)
            {
                case XmlNodeType.None:
                    break;
                case XmlNodeType.Element:
                    if (false && templateNode.InnerText != templateNode.InnerXml)
                    {
                        string ss = "";
                        foreach (XmlNode childNode in templateNode.ChildNodes)
                        {
                            ss = ss + " " + InnerXmlText(childNode);
                        }
                        //  return ss;
                    }
                    if (true) return templateNode.InnerXml;

                    string innerText = templateNode.InnerText.Trim();
                    if (templateNode.InnerXml.Length >= templateNode.InnerText.Length)
                    {
                        if (innerText.Length == 0)
                        {
                            //writeToLog("return empty?");
                        }
                        return templateNode.InnerXml;
                    }

                    return templateNode.InnerXml;
                    break;
                case XmlNodeType.Attribute:
                    break;
                case XmlNodeType.Text:
                    if (templateNode.InnerXml.Length > 0)
                    {
                        return templateNode.InnerText + TextNodeValue(templateNode);
                    }
                    return templateNode.InnerText;

                case XmlNodeType.CDATA:
                    break;
                case XmlNodeType.EntityReference:
                    break;
                case XmlNodeType.Entity:
                    {
                        string ss = "";
                        foreach (XmlNode childNode in templateNode.ChildNodes)
                        {
                            ss = ss + " " + InnerXmlText(childNode);
                        }
                        return ss;
                    }
                    break;
                case XmlNodeType.ProcessingInstruction:
                    break;
                case XmlNodeType.Comment:
                    break;
                case XmlNodeType.Document:
                    break;
                case XmlNodeType.DocumentType:
                    break;
                case XmlNodeType.DocumentFragment:
                    break;
                case XmlNodeType.Notation:
                    break;
                case XmlNodeType.Whitespace:
                    break;
                case XmlNodeType.SignificantWhitespace:
                    break;
                case XmlNodeType.EndElement:
                    break;
                case XmlNodeType.EndEntity:
                    break;
                case XmlNodeType.XmlDeclaration:
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            string s = String.Format("Unsurported Node Type {0} in {1}", templateNode.NodeType, templateNode.OuterXml);
            writeDebugLine(s);
            throw new ArgumentOutOfRangeException(s);
        }

        public static string TextNodeValue(XmlNode templateNode)
        {
            return TextNodeValue(templateNode, false);
        }
        public static string TextNodeValue(XmlNode templateNode, bool asSource)
        {
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                return templateNode.OuterXml;
            }
            string s = String.Format("Unsurported Node Type {0} in {1}", templateNode.NodeType, templateNode.OuterXml);
            writeDebugLine(s);
            throw new ArgumentOutOfRangeException(s);
        }

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        public static XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            return FindNode(name, node, ifMissing, 1);
        }
        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        public static XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing, int searchDepth)
        {
            bool searchChildren = searchDepth > 0;
            foreach (string n in NamesStrings(name))
            {
                if (NameMatches(node, n))
                {
                    return node;
                }
            }
            if (!searchChildren) return ifMissing;
            int nsearchDepth = searchDepth - 1;
            foreach (XmlNode child in node.ChildNodes)
            {
                foreach (string n in NamesStrings(name))
                {
                    if (NameMatches(child, n))
                    {
                        return child;
                    }
                }
                if (searchDepth > 0)
                {
                    var cnode = FindNode(name, child, null, nsearchDepth);
                    if (cnode != null) return cnode;
                }
            }
            return ifMissing;
        }

        public static string FindNodeOrAttrib(XmlNode myNode, string names, Func<string> defaultNotFound)
        {
            const string attribNotFOund = "ATTRIB_NOT_FOUND";
            string value = GetAttribValue(myNode, names, attribNotFOund);
            if (value == attribNotFOund)
            {
                XmlNode holder = FindNode(names, myNode, null);
                if (holder != null)
                {
                    value = InnerXmlText(holder);
                    return value;
                }
                return defaultNotFound == null ? null : defaultNotFound();
            }
            return value;
        }

        public static XmlNode FindNodeOrHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (string n in NamesStrings(name))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (NameMatches(child, n))
                    {
                        return child;
                    }
                }
                if (node.Name == name) return node;
                if (NameMatches(node, name))
                {
                    return node;
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }

        public static XmlNode FindHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (string n in NamesStrings(name))
            {
                if (NameMatches(node, n))
                {
                    return node;
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }

        public static List<XmlNode> FindNodes(string name, XmlNode node)
        {
            var nodes = new List<XmlNode>();
            FindNodes(name, node, nodes, 1);
            return nodes;
        }
        public static bool FindNodes(string name, XmlNode node, List<XmlNode> nodes, int searchDepth)
        {
            foreach (string n in NamesStrings(name))
            {
                bool searchChildren = searchDepth > 0;
                if (!searchChildren) return false;
                int nsearchDepth = searchDepth - 1;
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (NameMatches(child, n))
                    {
                        nodes.Add(child);
                    }
                    else
                    {
                        FindNodes(name, child, nodes, nsearchDepth);
                    }
                }
            }
            return nodes.Count > 0;
        }

        public static int NonAlphaCount(string input)
        {
            input = CleanWhitepaces(input);
            int na = 0;
            foreach (char s in input)
            {
                if (Char.IsLetterOrDigit(s)) continue;
                na++;
            }
            return na;
        }


        public static string ParentTextAndSourceInfo(XmlNode element)
        {
            return TextAndSourceInfo(element.ParentNode ?? element) + " " + LocationEscapedInfo(element);
        }

        public static T NodeInfo<T>(XmlNode templateNode, Func<string, XmlNode, T> funct)
        {
            T s = default(T);
            XmlNode nxt = templateNode;
            s = funct("same", nxt);
            if (!ReferenceEquals(s, null)) return s;
            nxt = templateNode.NextSibling;
            s = funct("next", nxt);
            if (!ReferenceEquals(s, null)) return s;
            nxt = templateNode.PreviousSibling;
            s = funct("prev", nxt);
            if (!ReferenceEquals(s, null)) return s;
            nxt = templateNode.ParentNode;
            s = funct("prnt", nxt);
            if (!ReferenceEquals(s, null)) return s;
            return s;
        }

        public static string LocationEscapedInfo(XmlNode templateNode)
        {
            return CommentEscapedInfo(LocationInfo(templateNode));
        }

        public static string CommentEscapedInfo(string templateNode)
        {
            return "<!-- " + templateNode + " -->";
        }

        public static readonly IXmlLineInfo NoLineNumberInfoZeroZero = new LineNumberInfoZeroZero();
        public static IXmlLineInfo ToLineInfo(XmlNode templateNode)
        {
            var li = ToLineInfo(templateNode, true);
            if (li != null) if (li.HasLineInfo()) return li;
            li = ToLineInfo(templateNode, false);
            if (li != null) if (li.HasLineInfo()) return li;
            return NoLineNumberInfoZeroZero;
        }
        public static IXmlLineInfo ToLineInfo(XmlNode templateNode, bool noParents)
        {
            if (templateNode == null) return NoLineNumberInfoZeroZero;
            IXmlLineInfo li = templateNode as IXmlLineInfo;
            if (li != null)
            {
                if (li.HasLineInfo())
                {
                    if (li.LineNumber > 0 || li.LinePosition > 0)
                    {
                        return li;
                    }

                }
            }
            if (noParents)
            {
                foreach (var v in templateNode.ChildNodes)
                {
                    li = ToLineInfo(v as XmlNode, true);
                    if (li != null) if (li.HasLineInfo()) return li;
                }
                return NoLineNumberInfoZeroZero;
            }
            return ToLineInfo(templateNode.ParentNode, false);
        }

        public static string LocationInfo(XmlNode templateNode)
        {
            return FileNameOfXmlNode(templateNode) + ":" + GetLineNumberOfXmlNode(templateNode);
        }

        public static string GetLineNumberOfXmlNode(IXmlLineInfo impl1)
        {
            if (impl1 == null) return "(-1,-1)";
            return string.Format("({0},{1})", impl1.LineNumber, impl1.LinePosition);
        }

        public static string GetLineNumberOfXmlNode(XmlNode templateNode)
        {
            return NodeInfo<string>(templateNode, LineNoInfo) ?? "(-1,-1)";
        }

        public static string FileNameOfXmlNode(XmlNode templateNode)
        {
            string doc = NodeInfo(templateNode,
                                  (
                                      (strng, node) =>
                                      {
                                          if (node == null) return null;
                                          XmlDocument od = node.OwnerDocument;
                                          if (od == null) return null;
                                          var doc3 = od as IHasFilename;
                                          if (doc3 != null) return doc3.Filename;
                                          var doc2 = od as XmlDocumentLineInfo;
                                          if (doc2 != null) return doc2.InfoString;
                                          string st = od.ToString().Trim();
                                          if (st.Length == 0) return null;
                                          return st;
                                      }));
            if (doc == null)
            {
                doc = "nodoc";
            }
            return doc;
        }

        public static string LineNoInfo(string where, XmlNode templateNode)
        {
            string s = null;
            LineInfoElementImpl li = templateNode as LineInfoElementImpl;
            if (li != null)
            {
                if (li.LineNumber != 0 && li.LinePosition != 0)
                {
                    return "(" + li.LineNumber + "," + li.LinePosition + ") ";
                }
                XmlNode Parent = li.ParentNode;
                if (Parent != null && Parent != li)
                {
                    s = LineNoInfo(where + ".prnt", Parent);
                }
                if (s == null)
                {
                    if (li.LineNumber != 0 || li.LinePosition != 0)
                    {
                        return "(" + li.LineNumber + "," + li.LinePosition + ") ";
                    }
                }
            }
            return s;
        }

        public static string TextAndSourceInfo(XmlNode templateNode)
        {
            return TextInfo(templateNode) + " " + LocationEscapedInfo(templateNode);
        }

        public static string TextInfo(XmlNode templateNode)
        {
            XmlNode textNode = templateNode; //.ParentNode ?? templateNode;
            string s = CleanWhitepaces(textNode.OuterXml);
            if (String.IsNullOrEmpty(s))
            {
                XmlNode Parent = templateNode.ParentNode;
                if (Parent != null && Parent != templateNode)
                {
                    return TextInfo(Parent);
                }
                return textNode.OuterXml;
            }
            return s;
        }

        public static LineInfoElementImpl CopyNode(XmlNode node, bool copyParent)
        {
            if (copyParent)
            {
                XmlNode parentNode = node.ParentNode as XmlElement;
                if (parentNode != null)
                {
                    LineInfoElementImpl xmlNode0 = node as LineInfoElementImpl;
                    if (xmlNode0 != null)
                    {
                        int idx = xmlNode0.IndexInBaseParent;
                        if (idx >= 0)
                        {
                            XmlNode parentCopy = parentNode.CloneNode(true);
                            LineInfoElementImpl.unsetReadonly(parentCopy);
                            //parentCopy.ReadOnly = xmlNode0.ReadOnly;
                            XmlNode obj = parentCopy.ChildNodes[idx];
                            LineInfoElementImpl.unsetReadonly(obj);
                            return (LineInfoElementImpl)obj;
                        }
                    }
                    writeDebugLine("cannot copy nodes parent " + node);
                }
            }

            XmlNode oc = node.CloneNode(true);

            LineInfoElementImpl xmlNode = (LineInfoElementImpl)(oc as IXmlLineInfo);
            if (xmlNode == null)
            {
                xmlNode = (LineInfoElementImpl)getNodeAndSetSibling(node.OuterXml, false, false, node);
                LineInfoElementImpl.unsetReadonly(xmlNode);
            }
            else
            {
                LineInfoElementImpl.unsetReadonly(xmlNode);
            }
            LineInfoElementImpl.unsetReadonly(xmlNode);
            return xmlNode;
        }


        public static LineInfoElementImpl CopyNode(string newName, XmlNode node, bool copyParent)
        {
            XmlDocument od = node.OwnerDocument;
            LineInfoElementImpl newnode =
                (LineInfoElementImpl)node.OwnerDocument.CreateNode(node.NodeType, newName, node.NamespaceURI);
            newnode.ReadOnly = false;
            newnode.SetParentFromNode(node);
            newnode.lParent = ToLineInfoElement(node.ParentNode);
            XmlAttributeCollection ats = node.Attributes;
            if (ats != null)
                foreach (XmlAttribute a in ats)
                {
                    XmlAttribute na = od.CreateAttribute(a.Prefix, a.LocalName, a.NamespaceURI);
                    na.Value = a.Value;
                    newnode.Attributes.Append(na);
                }
            foreach (XmlNode a in node.ChildNodes)
            {
                newnode.AppendChild(a.CloneNode(true));
            }
            newnode.ReadOnly = node.IsReadOnly;
            return (LineInfoElementImpl)newnode;
        }

        public static LineInfoElementImpl ToLineInfoElement(XmlNode pattern)
        {
            if (pattern == null) return null;
            if (pattern is LineInfoElementImpl)
            {
                return (LineInfoElementImpl)pattern;
            }
            return CopyNode(pattern, true);
        }

        public static T GetAttribValue<T>(XmlNode templateNode, string attribName, T defaultIfEmpty)
            where T : IConvertible
        {
            return GetAttribValue(templateNode, attribName, () => (defaultIfEmpty), null);
        }

        public static string GetAttribValue(XmlNode templateNode, string attribName, string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, null);
        }

        public static T GetAttribValue<T>(XmlNode node, string attribName, Func<T> defaultIfEmpty,
                                          Func<IConvertible, T> sq) where T : IConvertible
        {
            string realName;
            return GetAttribValue(node, attribName, out realName, defaultIfEmpty, sq);
        }

        public static T GetAttribValue<T>(XmlNode node, string attribName, out string realName,
                                          Func<T> defaultIfEmpty, Func<IConvertible, T> sq) where T : IConvertible
        {
            if (sq == null) sq = null; // PASSTHRU;
            realName = null;
            if (node == null) return defaultIfEmpty();
            bool found = false;
            T u = default(T);
            if (node.Attributes == null) return defaultIfEmpty();
            foreach (string nameS in NamesStrings(attribName))
            {
                String attribnym = nameS.ToLower();
                foreach (XmlAttribute attrib in node.Attributes)
                {
                    if (SearchStringMatches(attribnym, attrib.Name))
                    {
                        found = true;
                        realName = attribnym;
                        if (sq == null) sq = PASSTHRU<T>;
                        T r = sq(attrib.Value);
                        if (!ReferenceEquals(r, null)) return r;
                        // ReSharper disable ConditionIsAlwaysTrueOrFalse
                        if (ReferenceEquals(r, null))
                        {
                            u = PASSTHRU<T>(attrib.Value);
                            continue;
                        }
                        // ReSharper restore ConditionIsAlwaysTrueOrFalse
                        u = r;
                    }
                }
            }
            if (found) return u;
            return defaultIfEmpty();
        }

        public static T PASSTHRU<T>(IConvertible arg) where T : IConvertible
        {
            Type solid = typeof(T);
            if (solid.IsInstanceOfType(arg))
            {
                return (T)arg;
            }
            if (ReferenceEquals(arg, null))
            {
                return default(T);
            }
            return (T)FormatProviderConvertor(arg, solid);
        }

        public static T GetAttribValueDouble<T>(XmlNode node, string attribName, T defaultIfEmpty,
                                                Func<string, T> reduceStar) where T : IConvertible
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName)
                {
                    T rs = reduceStar(attrib.Value);
                    try
                    {
                        return (T)rs.ToType(typeof(T), null);
                    }
                    catch (Exception exception)
                    {
                        writeDebugLine("XMLTRACE: DECIMAL " + reduceStar + " " + exception);
                    }
                }
            }
            return defaultIfEmpty;
        }

        public static void writeDebugLine(string format, params object[] args)
        {
            DLRConsole.DebugWriteLine(format, args);
        }

        public static bool TryParseBool(XmlNode templateNode, string attribName, out bool tf)
        {
            return TryParseBool(GetAttribValue<string>(templateNode, attribName, null), out tf);
        }
        /// <summary>
        /// Foreach XML Node in he collection search for the value 
        /// </summary>
        /// <param name="templateNodes"></param>
        /// <param name="attribName"></param>
        /// <param name="tf"></param>
        /// <returns></returns>
        public static bool TryParseBool(ICollection<XmlNode> templateNodes, string attribName, out bool tf)
        {
            foreach (XmlNode templateNode in templateNodes)
            {
                //if (TryParseBool(GetAttribValue<string>(templateNode, attribName, null), out tf))
                if (TryParseBool(templateNode, attribName, out tf))
                {
                    return true;
                }
            }
            tf = default(bool);
            return false;
        }

        public static bool TryParseBool(string parse, out bool tf)
        {
            if (String.IsNullOrEmpty(parse))
            {
                tf = default(Boolean);
                return false;
            }
            parse = ToUpper(parse);
            if (IsFalseOrNo(parse))
            {
                tf = false;
                return true;
            }
            if (IsTrueOrYes(parse))
            {
                tf = true;
                return true;
            }
            tf = default(Boolean);
            return false;
        }

        public static bool IsFalseOrNo(string tst)
        {
            return (tst == "NO" || tst == "N" || tst == "FALSE" || tst == "F" ||
                    tst == "NIL" || tst == "()" || (tst.StartsWith("<!--") && tst.Contains("FAIL")));
        }

        public static bool IsTrueOrYes(string tst)
        {
            return (tst == "YES" || tst == "Y" || tst == "TRUE" || tst == "T" ||
                    (tst.StartsWith("<!--") && tst.Contains("SUCCESS")));
        }


        /// <summary>
        /// Helper method that converts passed string into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static XmlNode ParseNode(XmlDocumentLineInfo doc, TextReader sr, string outerXML)
        {
            try
            {
                if (DontFragment)
                {
                    doc = doc.GetReusableDoc();
                    doc.SetNodesReadOnly = false;
                    doc.IsFile = false;
                }
                else
                {
                    if (!doc.WasUsed)
                    {
                        doc.LoadXml("<items/>");
                    }
                    var parentNode = doc.DocumentElement;
                    var frag = doc.CreateDocumentFragment();
                    frag.InnerXml = outerXML ?? sr.ReadToEnd();
                    if (frag.ChildNodes.Count != 1)
                    {
                        return frag;
                    }
                    XmlElement newNode = frag.LastChild as XmlElement;
                    if (newNode != null) return newNode;
                    parentNode.AppendChild(frag);
                    var ret = parentNode.LastChild ?? frag;
                    return ret;
                }
                if (outerXML == null)
                {
                    outerXML = sr.ReadToEnd();
                    sr = new StringReader(outerXML);
                }
                doc.Load(sr);
                int childCount = doc.ChildNodes.Count;
                if (childCount == 0)
                {
                    writeDebugLine("ERROR NULL_CHILDS outerXML='{0}'", outerXML);
                    return null;
                }
                if (childCount != 1)
                {
                    writeDebugLine("ERROR WRONG_NUMBER_OF_CHILDS {0} outerXML='{1}'", childCount, outerXML);
                    return doc;
                }
                XmlNode temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    return temp;
                }
                return temp;
            }
            catch (XmlException)
            {
                throw;
            }
            catch (Exception exception)
            {
                writeDebugLine("outerXML=" + outerXML);
                throw;
            }
            finally
            {
                doc.DiscardReaders();
            }
        }

        /// <summary>
        /// Helper method that converts passed string into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static XmlNode getNode(string outerXML, bool wasWrapped)
        {
            XmlDocumentLineInfo stringOnlyDoc1 = StringOnlyDoc;
            stringOnlyDoc1.SetText(outerXML);
            return getDocNode(outerXML, wasWrapped, stringOnlyDoc1);
        }

        public static XmlNode getNode(string outerXML)
        {
            return getDocNode(outerXML, false, false, StringOnlyDoc);
        }

        public static XmlNode getNodeAndSetSiblingNode(string outerXML, XmlNode sibling)
        {
            return getNodeAndSetSibling(outerXML, false, true, sibling);
        }


        public static bool UseOneStringOnlyDoc = false;
        public static XmlDocumentLineInfo StringOnlyDoc
        {
            get
            {
                if (UseOneStringOnlyDoc)
                {
                    var doc = new XmlDocumentLineInfo();
                    doc.InfoString = "new StringOnlyDoc";
                    return doc;
                }
                return stringOnlyDoc;
            }
        }
        public static XmlNode getDocNode(string outerXML, bool wasWrapped, XmlDocument doc0)
        {
            return getDocNode(outerXML, wasWrapped, !wasWrapped, doc0);
        }

        public static XmlNode getDocNode(string outerXML, bool wasWrapped, bool mayTryToWrap, XmlDocument doc0Hint)
        {
            var doc0 = doc0Hint ?? StringOnlyDoc;
            XmlDocumentLineInfo doc = doc0 as XmlDocumentLineInfo;
            if (doc == null && doc0 != null) doc = doc0.OwnerDocument as XmlDocumentLineInfo;
            outerXML = ReTrimAndspace(outerXML);
            try
            {
                if (!wasWrapped && outerXML.Length > 0 && mayTryToWrap)
                {
                    var ch = outerXML[0];
                    if (ch != '<' && ch != '&')
                    {
                        outerXML = XXXX(outerXML);
                        wasWrapped = true;
                    }
                }
                if (outerXML.Contains("<BOT "))
                {

                }
                var node = ParseNode(doc, new StringReader(outerXML), outerXML);
                return ResultNode(node, wasWrapped);

            }
            catch (XmlException exception)
            {
                writeDebugLine("ERROR NODE-IFYING " + outerXML);
                if (!wasWrapped && mayTryToWrap)
                {
                    outerXML = XXXX(outerXML);
                    var node = ParseNode(doc, new StringReader(outerXML), outerXML);
                    return ResultNode(node, true);
                }
                throw;
            }
            catch (Exception exception)
            {
                writeDebugLine("ERROR outerXML='" + outerXML + "'\n" + exception + "\n");
                throw;
            }
            finally
            {
                doc.DiscardReaders();
            }
        }


        public static string ReplaceMap(string strTrim, string[][] pairs)
        {
            foreach (string[] pair in pairs)
            {
                string pair1 = pair[0];
                string pair2 = pair[1];
                if (strTrim.Contains(pair1))
                {
                    strTrim = ReplaceAll(strTrim, pair1, pair2);
                    if (strTrim.Contains(pair1))
                    {
                        strTrim = strTrim.Replace(pair1, pair2);
                    }
                }
            }
            return strTrim;
        }
        public static string ReplacePairs(string strTrim, params string[] pairs)
        {
            int index = 0;
            int pairsLength = pairs.Length;
            while (index < pairsLength)
            {
                strTrim = ReplaceAll(strTrim, pairs[index++], pairs[index++]);
            }
            return strTrim;
        }

        public static string ReplaceAll(string source, string b, string a)
        {
            string sourceReplace = source.Replace(b, a);
            var result = OlderReference(source, sourceReplace);
            while (result.Contains(b))
            {
                sourceReplace = result.Replace(b, a);
                result = OlderReference(source, sourceReplace);
            }
            return result;
        }

        public static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            var s = OlderReference(substitute, substitute.Replace("> ", ">").Replace(" <", "<").Replace("  ", " "));
            if (s.Length == 1)
            {
                if (s != " ") return s;
                return s;
            }
            return Trim(substitute);
        }

        public static string Trim00(string outerXml)
        {
            return CleanWhitepaces(outerXml);
        }

        public static string ToUpper(string param1)
        {
            var outp = param1.ToUpper();
            return OlderReference(param1, outp);
        }

        public static string ToLower(string param1)
        {
            var outp = param1.ToLower();
            return OlderReference(param1, outp);
        }
        public static string Trim(string param1)
        {
            var outp = param1.Trim();
            return OlderReference(param1, outp);
        }
        public static string ConsolidSpaces(string param1)
        {
            var outp = param1.Replace("  ", " ");
            return OlderReference(param1, outp);
        }

        /// <summary>
        /// Helps keep new strings garbage collectable by using the previous
        /// version if they are the same string
        /// </summary>
        /// <param name="before"></param>
        /// <param name="after"></param>
        /// <returns></returns>
        public static string OlderReference(string before, string after)
        {
            if (after == before)
            {
                return before;
            }
            return after;
        }

        public static string SafeFormat(string fmt, params object[] args)
        {
            return DLRConsole.SafeFormat(fmt, args);
        }

        public static XmlNode ResultNode(XmlNode node, bool wasWrapped)
        {
            if (wasWrapped)
            {
                if (node.ChildNodes.Count == 1)
                {
                    var prevSib0 = node.PreviousSibling;
                    node = node.FirstChild;
                    return node;
                }
                node = node.FirstChild.ParentNode;
                return node;
            }
            var prevSib = node.PreviousSibling;
            return node;
        }


        public static XmlNode getNodeAndSetSibling(string outerXML, bool wasWrapped, bool mayTryToWrap, XmlNode nodeHint)
        {
            var templateNode = nodeHint as XmlLinkedNode;
            var ownerDoc = nodeHint as XmlDocumentLineInfo;
            if (ownerDoc != null) templateNode = null;
            try
            {
                ownerDoc = ownerDoc ?? nodeHint.OwnerDocument as XmlDocumentLineInfo;
                XmlNode temp =
                    getDocNode(outerXML,
                               wasWrapped, mayTryToWrap,
                               ownerDoc ?? StringOnlyDoc);
                if (temp is LineInfoElementImpl)
                {
                    LineInfoElementImpl li = (LineInfoElementImpl)temp;
                    li.SetParentFromNode(templateNode);
                    return temp; //.FirstChild;}
                }
                if (temp is XmlSourceLineInfo)
                {
                    XmlSourceLineInfo li = (XmlSourceLineInfo)temp;
                    li.SetParentFromNode(templateNode);
                    return temp; //.FirstChild;}
                }
                return temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                writeDebugLine("ERROR outerXML='" + outerXML + "'\n" + exception + "\n" +
                               LocationInfo(templateNode));
                throw;
            }
        }

        private static XmlNode GetXmlTemp(string outerXML, XmlNode templateNode, StringReader sr)
        {
            string named = "From " + outerXML;
            if (templateNode != null)
            {
                named = "" + templateNode.OwnerDocument;
                if (String.IsNullOrEmpty(named)) named = "from: " + templateNode.OuterXml;
            }
            XmlDocumentLineInfo doc = null; // templateNode.OwnerDocument as XmlDocumentLineInfo;
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (doc == null)
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
            {
                doc = new XmlDocumentLineInfo(named, true);
            }
            doc.Load(sr);
            XmlElement de = doc.DocumentElement;
            //doc.IsReadOnly = false;
            if (doc.ChildNodes.Count == 0)
            {
                writeDebugLine("NULL outerXML=" + outerXML);
                //  return null;
            }
            if (doc.ChildNodes.Count != 1)
            {
                writeDebugLine("1 != outerXML=" + outerXML);
            }
            return doc.FirstChild;
        }


        public static string CleanWhitepaces(string xml2)
        {
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }

        public static bool Unused(char arg1, char arg2)
        {
            throw new NotImplementedException();
            return false;
        }


        public static string CleanWhitepaces(string xml2, string padchars,
                                             Func<char, char, bool> ifBefore, Func<char, char, bool> ifAfter)
        {
            if (xml2 == null) return xml2;
            const long maxCleanSize = 2 << 14;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize || inlen < 2)
            {
                return xml2;
            }

            if (padchars != null)
            {
                if (xml2.IndexOfAny(padchars.ToCharArray(), 0) == -1 || (xml2.IndexOfAny(new[] { '\\', ':', '/' }, 0) > 0))
                {
                    padchars = null;
                }
            }

            StringBuilder s = new StringBuilder(inlen);

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            int charIndex = -1;
            foreach (char c0 in xml2)
            {
                charIndex++;
                if (c0 <= 32)
                {
                    if (inwhite)
                    {
                        chgd = true;
                        continue;
                    }
                    inwhite = true;
                    pendingWhitespace = true;
                    continue;
                }
                switch (c0)
                {
                    case '/':
                        if (lastChar == 'r')
                        {
                            xmlFound = true;
                        }
                        int nxtIndex = charIndex + 1;
                        if (nxtIndex < inlen && xml2[nxtIndex] == '>')
                        {
                            if (!pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = true;
                            }
                        }
                        else
                        {
                            inwhite = true;
                            if (pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = false;
                            }
                        }
                        break;
                    case '>':
                    case '<':
                    case '\\':
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    default:
                        if (padchars != null)
                        {
                            bool before = padchars.Contains("" + lastChar);
                            if (before && ifBefore(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                            bool after = padchars.Contains("" + c0);
                            if (after && ifAfter(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }
                        }
                        inwhite = false;
                        break;
                }
                if (pendingWhitespace)
                {
                    s.Append(' ');
                    pendingWhitespace = false;
                }
                s.Append(c0);
                lastChar = c0;
            }
            if (pendingWhitespace) chgd = true;
            int len = s.Length;
            if (xmlFound)
            {
                s = s.Replace("<sr />", "<srai><star /></srai>");
                //s = s.Replace("star/>", "star index=\"1\"/>");
                if (len != s.Length)
                {
                    chgd = true;
                }
            }
            if (!chgd)
            {
                if (len != inlen)
                {
                    return s.ToString();
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s.ToString();
        }

        public static string nodeStartXML(XmlElement node)
        {
            //StringBuilder sb = new StringBuilder();
            //XmlWriter writer = XmlWriter.Create(sb);
            // writer.Write
            string remove = node.InnerXml;
            string outer = node.OuterXml;
            if (remove == "") return outer;
            int idx = outer.IndexOf(remove);
            if (idx > 1)
            {
                return outer.Substring(0, idx);
            }
            return outer.Replace(remove, "");
        }

        public static char[] isValueSetChars = " ".ToCharArray();
        public static string isValueSetStart = "+++";
        public static int isValueSetSkip = isValueSetStart.Length;
        public static readonly XmlDocumentLineInfo stringOnlyDoc = new XmlDocumentLineInfo("getNode(ANYTHING)", false);
        public static bool DontFragment = true;
        public static string XXXX(string res)
        {
            if (!res.EndsWith("</xxxx>"))
            {
                return "<xxxx>" + res + "</xxxx>";
            }
            else
            {
                // return "<xxxx>" + res + "</xxxx>";
                return res;
            }
        }

        public static string MakeXmlCommentSafe(string unescapedStuff)
        {
            if (string.IsNullOrEmpty(unescapedStuff)) return "";
            return ReplacePairs(unescapedStuff, "<!--", "<#--", "-->", "--#>");
        }

        public static void SetLineInfo(XmlNode value0, IXmlLineInfo info, string filename)
        {
            var value = value0 as XmlSourceLineInfo;
            if (value != null)
            {
                value.SetLineInfo(info.LineNumber, info.LinePosition, filename);

            }
            var value1 = value0 as XmlSourceInfo;
            if (value1 != null)
            {
                //value1.SetOwnerDocument();
            }
        }
    }

    internal class LineNumberInfoZeroZero : IXmlLineInfo
    {
        /// <summary>
        /// Gets a value indicating whether the class can return line information.
        /// </summary>
        /// <returns>
        /// true if <see cref="P:System.Xml.IXmlLineInfo.LineNumber"/> and <see cref="P:System.Xml.IXmlLineInfo.LinePosition"/> can be provided; otherwise, false.
        /// </returns>
        public bool HasLineInfo()
        {
            return false;
        }

        public int LineNumber
        {
            get { return 0; }
        }

        public int LinePosition
        {
            get { return 0; }
        }
    }
}
