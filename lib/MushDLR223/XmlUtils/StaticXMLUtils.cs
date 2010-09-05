using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;

namespace MushDLR223.Utilities
{
    //public delegate string ReduceStar0(string value);

    public class StaticXMLUtils
    {
        public static IFormatProvider FormatProvider;
        public static Func<IConvertible, Type, IConvertible> FormatProviderConvertor;

        protected static string[] NamesStrings(string name)
        {
            return name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        }

        protected static bool NameMatches(XmlNode node, string s)
        {
            return node.Name.ToLower() == s || node.LocalName.ToLower() == s;
        }


        public static bool ContainsXml(string unifiable)
        {
            String s = unifiable;
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&"))
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
            xml1 = CleanWhitepaces(xml1).ToLower();
            xml2 = CleanWhitepaces(xml2).ToLower();
            if (xml1.Length != xml2.Length) return false;
            if (xml1 == xml2) return true;
            if (xml1.ToUpper() == xml2.ToUpper())
            {
                return true;
            }
            return false;
        }

        public static bool IsHtmlTag(string name)
        {
            return " html head body font pre p div br dd td th tr table frame frameset &ltl &gt; input option select  "
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


        protected static string ToVisiable(XmlNode node)
        {
            string oxml = node.OuterXml;
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


        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        public static XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            name = name.ToLower();
            foreach (string n in NamesStrings(name))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (NameMatches(child, n))
                    {
                        return child;
                    }
                }
            }
            return ifMissing;
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
            name = name.ToLower();
            if (name.Contains(","))
                throw new NotImplementedException("Commans in FindNodes " + name + " in " + node.OuterXml);
            var nodes = new List<XmlNode>();
            foreach (XmlNode child in node.ChildNodes)
            {
                if (NameMatches(child, name))
                {
                    nodes.Add(child);
                }
            }
            return nodes;
        }

        protected static int NonAlphaCount(string input)
        {
            input = CleanWhitepaces(input);
            int na = 0;
            foreach (char s in input)
            {
                if (char.IsLetterOrDigit(s)) continue;
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
            return "<!-- " + LocationInfo(templateNode) + " -->";
        }

        public static string LocationInfo(XmlNode templateNode)
        {
            string lines = NodeInfo<string>(templateNode, LineNoInfo) ?? "(-1,-1)";
            string doc = NodeInfo(templateNode,
                                  (
                                      (strng, node) =>
                                          {
                                              if (node == null) return null;
                                              XmlDocument od = node.OwnerDocument;
                                              if (od == null) return null;
                                              string st = od.ToString().Trim();
                                              if (st.Length == 0) return null;
                                              return st;
                                          }));
            if (doc == null)
            {
                doc = "nodoc";
            }
            return doc + ":" + lines;
        }

        private static string LineNoInfo(string where, XmlNode templateNode)
        {
            string s = null;
            var li = templateNode as LineInfoElementImpl;
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
                XmlNode parentNode = node.ParentNode;
                if (parentNode != null)
                {
                    var xmlNode0 = node as LineInfoElementImpl;
                    int idx = xmlNode0.IndexInBaseParent;
                    if (idx >= 0)
                    {
                        var parentCopy = (LineInfoElementImpl) parentNode.CloneNode(true);
                        parentCopy.ReadOnly = xmlNode0.ReadOnly;
                        xmlNode0 = (LineInfoElementImpl) parentCopy.ChildNodes[idx];
                        xmlNode0.ReadOnly = !copyParent;
                        return xmlNode0;
                    }
                }
            }

            XmlNode oc = node.CloneNode(true);

            var xmlNode = (LineInfoElementImpl) (oc as IXmlLineInfo);
            if (xmlNode == null)
            {
                xmlNode = (LineInfoElementImpl) getNode(node.OuterXml, node);
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
            var newnode =
                (LineInfoElementImpl) node.OwnerDocument.CreateNode(node.NodeType, newName, node.NamespaceURI);
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
            return newnode;
        }

        public static LineInfoElementImpl ToLineInfoElement(XmlNode pattern)
        {
            if (pattern == null) return null;
            if (pattern is LineInfoElementImpl)
            {
                return (LineInfoElementImpl) pattern;
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
            foreach (string nameS in attribName.Split(new[] {','}, StringSplitOptions.RemoveEmptyEntries))
            {
                String attribnym = nameS.ToLower();
                foreach (XmlAttribute attrib in node.Attributes)
                {
                    if (attrib.Name.ToLower() == attribnym)
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
            T output = default(T);
            if (ReferenceEquals(arg, null))
            {
                return output;
            }
            Type solid = typeof (T);
            if (solid.IsInstanceOfType(arg))
            {
                return (T) arg;
            }
            return (T) FormatProviderConvertor(arg, solid);
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
                        return (T) rs.ToType(typeof (T), null);
                    }
                    catch (Exception exception)
                    {
                        writeDebugLine("AIMLTRACE: DECIMAL " + reduceStar + " " + exception);
                    }
                }
            }
            return defaultIfEmpty;
        }

        private static void writeDebugLine(string s)
        {
            throw new NotImplementedException();
        }

        public static bool TryParseBool(XmlNode templateNode, string attribName, out bool tf)
        {
            return TryParseBool(GetAttribValue<string>(templateNode, attribName, null), out tf);
        }

        public static bool TryParseBool(string parse, out bool tf)
        {
            if (string.IsNullOrEmpty(parse))
            {
                tf = default(Boolean);
                return false;
            }
            parse = parse.ToUpper();
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
            return (tst == "NO" || tst == "N" || tst == "FALSE" || tst == "F" || tst == "NIL");
        }

        public static bool IsTrueOrYes(string tst)
        {
            return (tst == "YES" || tst == "Y" || tst == "TRUE" || tst == "T");
        }


        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static XmlNode getNode(string outerXML)
        {
            var sr = new StringReader(outerXML);
            try
            {
                var doc = new XmlDocumentLineInfo("From " + outerXML, false);
                doc.Load(sr);
                if (doc.ChildNodes.Count == 0)
                {
                    writeDebugLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    writeDebugLine("1 != outerXML=" + outerXML);
                }
                XmlNode temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    var li = (LineInfoElementImpl) temp;
                    return temp; //.FirstChild;}
                }
                return getNode("<node>" + outerXML + "</node>");
                //return (LineInfoElement)temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                writeDebugLine("outerXML=" + outerXML);
                throw;
            }
        }

        public static XmlNode getNode(string outerXML, XmlNode templateNode)
        {
            var sr = new StringReader(outerXML);
            try
            {
                string named = "From " + outerXML;
                if (templateNode != null)
                {
                    named = "" + templateNode.OwnerDocument;
                    if (string.IsNullOrEmpty(named)) named = "from: " + templateNode.OuterXml;
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
                XmlNode temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    var li = (LineInfoElementImpl) temp;
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


        public static string CleanWhitepaces(string xml2)
        {
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }

        protected static bool Unused(char arg1, char arg2)
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
            if (inlen > maxCleanSize)
            {
                return xml2;
            }

            bool padWildCards = true;

            padWildCards = xml2.IndexOfAny("\\:/".ToCharArray(), 0) == -1;

            if (!padWildCards) padchars = null;

            var s = new StringBuilder(inlen);

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            foreach (char c0 in xml2)
            {
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
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
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
                s = s.Replace("<sr/>", "<srai><star index=\"1\"/></srai>");
                s = s.Replace("star/>", "star index=\"1\"/>");
                if (len != s.Length) chgd = true;
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
    }
}