using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;

namespace RTParser.Utils
{
    public class StaticXMLUtil
    {
        public static string CleanWhitepaces(object info)
        {
            if (info is XmlNode)
            {
                XmlNode n = (XmlNode)info;
                if (n.Name == "template") info = n.ParentNode;
            }
            if (info is TemplateInfo)
            {
                info = ((TemplateInfo)info).CategoryInfo;
            }
            return CleanWhitepaces("" + info);
        }


        public static string CleanWhitepacesLower(string xml2)
        {
            if (xml2 == null) return xml2;
            return CleanWhitepaces(xml2).ToLower().Replace(".", "").Replace("?", "").Replace("!", "");
        }

        public static bool ContainsAiml(Unifiable unifiable)
        {
            String s = unifiable.AsString();
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&"))
            {
                return true;
            }
            return false;
        }

        public static string MatchKeyClean(Unifiable unifiable)
        {
            return MatchKeyClean(unifiable.AsString());
        }

        public static string MatchKeyClean(string s)
        {
            s = CleanWhitepaces(s);
            if (s == "")
            {
                return "*";
            }
            return s;
        }

        static public int FromInsideLoaderContext(XmlNode currentNode, Request request, SubQuery query, Func<int> doit)
        {
            int total = 0;
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            RTPBot RProcessor = request.TargetBot;
            var prev = RProcessor.Loader;
            try
            {
                // RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();

                ThreadStart ts = StaticAIMLUtils.EnterTag(request, currentNode, query);
                try
                {
                    total += doit();
                }
                finally
                {
                    ts();
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
            return total;
        }


        protected static bool ContansNoInfo(Unifiable cond)
        {
            return cond == null || cond == Unifiable.STAR || cond == Unifiable.Empty;
        }


        internal static string GetNamedType(string name, string value)
        {
            if (value != null)
            {
                value = value.ToLower();
                if (value == "true" || value == "t" || value == "yes" || value == "y")
                {
                    return name;
                }
                if (value == "false" || value == "f" || value == "no" || value == "n")
                {
                    return null;
                }
                else
                {
                    return value;
                }
            }
            return value;
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


        static public bool IsInnerTextOnly(XmlNode node)
        {
            string s = node.InnerXml.Trim();
            if (s == node.InnerText.Trim())
            {
                if (s.Length > 0) return true;
                return true;
            }
            return false;
        }


        static protected string ToVisiable(XmlNode node)
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


        public static string CleanPunct(string normalizedPattern)
        {
            if (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith(".") || normalizedPattern.EndsWith("!"))
            {
                normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
            }
            return normalizedPattern;
        }

        protected static string NoWilds(string pattern)
        {
            pattern = pattern.Trim();
            int pl = pattern.Length;
            if (pl < 4) return pattern;
            while (pattern.Contains("*"))
            {
                pattern = pattern.Replace("*", " ").Trim();
            }
            return pattern;
        }

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        static public XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            name = name.ToLower();
            foreach (var n in NamesStrings(name))
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
        static public XmlNode FindNodeOrHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (var n in NamesStrings(name))
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
        static public XmlNode FindHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (var n in NamesStrings(name))
            {
                if (NameMatches(node, n))
                {
                    return node;
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }

        protected static string[] NamesStrings(string name)
        {
            return name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        }

        protected static bool NameMatches(XmlNode node, string s)
        {
            return node.Name.ToLower() == s || node.LocalName.ToLower() == s;
        }

        static public List<XmlNode> FindNodes(string name, XmlNode node)
        {
            name = name.ToLower();
            if (name.Contains(","))
                throw new NotImplementedException("Commans in FindNodes " + name + " in " + node.OuterXml);
            List<XmlNode> nodes = new List<XmlNode>();
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
            foreach (var s in input)
            {
                if (char.IsLetterOrDigit(s)) continue;
                na++;
            }
            return na;
        }

        //    #endregion

        public static bool AimlSame(string xml1, string xml2)
        {
            if (xml1 == xml2) return true;
            if (xml1 == null) return String.IsNullOrEmpty(xml2);
            if (xml2 == null) return String.IsNullOrEmpty(xml1);
            xml1 = CleanWhitepacesLower(xml1);
            xml2 = CleanWhitepacesLower(xml2);
            if (xml1.Length != xml2.Length) return false;
            if (xml1 == xml2) return true;
            if (xml1.ToUpper() == xml2.ToUpper())
            {
                return true;
            }
            return false;
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

        public static string CleanWildcards(string text)
        {
            if (text == null) return text;
            if (text.Contains("\""))
            {
                return CleanWhitepaces(text);
            }
            string clean = text;
            clean = CleanWhitepaces(text, "*",
                                    new Func<char, char, bool>((c0, c1) =>
                                                                   {
                                                                       if (char.IsLetterOrDigit(c1) || char.IsControl(c1)) return true;
                                                                       if ("\"'".Contains("" + c1)) return false;
                                                                       return false;
                                                                   }),
                                    new Func<char, char, bool>((c0, c1) =>
                                                                   {
                                                                       if (char.IsLetterOrDigit(c0) || char.IsControl(c0)) return true;
                                                                       if ("\"'".Contains("" + c0)) return false;
                                                                       return false;
                                                                   }));
            return clean;
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

        public static string ParentTextAndSourceInfo(XmlNode element)
        {
            return TextAndSourceInfo(element.ParentNode ?? element) + " " + LocationEscapedInfo(element);
        }

        public static string NodeInfo(XmlNode templateNode, Func<string, XmlNode, string> funct)
        {
            string s = null;
            XmlNode nxt = templateNode;
            s = funct("same", nxt);
            if (s != null) return s;
            nxt = templateNode.NextSibling;
            s = funct("next", nxt);
            if (s != null) return s;
            nxt = templateNode.PreviousSibling;
            s = funct("prev", nxt);
            if (s != null) return s;
            nxt = templateNode.ParentNode;
            s = funct("prnt", nxt);
            if (s != null) return s;
            return s;
        }

        public static string LocationEscapedInfo(XmlNode templateNode)
        {
            return "<!-- " + LocationInfo(templateNode) + " -->";
        }

        public static string LocationInfo(XmlNode templateNode)
        {
            string lines = NodeInfo(templateNode, LineNoInfo) ?? "(-1,-1)";
            string doc = NodeInfo(templateNode,
                                  (
                                      (strng, node) =>
                                          {
                                              if (node == null) return null;
                                              var od = node.OwnerDocument;
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
            }
            return s;
        }

        public static string TextAndSourceInfo(XmlNode templateNode)
        {
            return TextInfo(templateNode) + " " + LocationEscapedInfo(templateNode);
        }

        public static string TextInfo(XmlNode templateNode)
        {
            XmlNode textNode = templateNode;//.ParentNode ?? templateNode;
            string s = CleanWhitepaces(textNode.OuterXml);
            if (String.IsNullOrEmpty(s))
            {
                var Parent = templateNode.ParentNode;
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
                    LineInfoElementImpl xmlNode0 = node as LineInfoElementImpl;
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

            LineInfoElementImpl xmlNode = (LineInfoElementImpl) (oc as IXmlLineInfo);
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

        public static LineInfoElementImpl CopyNode (string newName, XmlNode node, bool copyParent) {
            var od = node.OwnerDocument;
            LineInfoElementImpl newnode =
                (LineInfoElementImpl)node.OwnerDocument.CreateNode(node.NodeType, newName, node.NamespaceURI);
            newnode.ReadOnly = false;
            newnode.SetParentFromNode(node);
            newnode.lParent = ToLineInfoElement(node.ParentNode);
            var ats = node.Attributes;
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
                return (LineInfoElementImpl) pattern;
            }
            return CopyNode(pattern, true);
        }

        public static bool IsHtmlTag(string name)
        {
            return " html head body font pre p div br dd td th tr table frame frameset &ltl &gt; input option select  "
                .Contains(" " + name.ToLower() + " ");
        }

        public static string GetAttribValue(XmlNode templateNode, string attribName, string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, null);
        }

        static public Unifiable GetAttribValue(XmlNode node, string attribName, Func<string> defaultIfEmpty, SubQuery sq)
        {
            string realName;
            return GetAttribValue(node, attribName, out realName, defaultIfEmpty, sq);
        }

        static public Unifiable GetAttribValue(XmlNode node, string attribName, out string realName, Func<string> defaultIfEmpty, SubQuery sq)
        {
            realName = null;
            if (node == null) return defaultIfEmpty();
            bool found = false;
            Unifiable u = Unifiable.NULL;
            if (node.Attributes == null) return defaultIfEmpty();
            foreach (var nameS in attribName.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries))
            {
                String attribnym = nameS.ToLower();
                foreach (XmlAttribute attrib in node.Attributes)
                {
                    if (attrib.Name.ToLower() == attribnym)
                    {
                        found = true;
                        realName = attribnym;
                        var r =  StaticAIMLUtils.ReduceStar(attrib.Value, sq, sq);
                        if (!Unifiable.IsNullOrEmpty(r)) return r;
                        if (Unifiable.IsNull(r)) continue;
                        u = r;
                    }
                }
            }
            if (found) return u;
            return defaultIfEmpty();
        }

        static public double GetAttribValue(XmlNode node, string attribName, double defaultIfEmpty, SubQuery sq)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName)
                {
                    string reduceStar = "" + StaticAIMLUtils.ReduceStar(attrib.Value, sq, sq);
                    try
                    {
                        return double.Parse(reduceStar);
                    }
                    catch (Exception exception)
                    {

                        RTPBot.writeDebugLine("AIMLTRACE: DECIMAL " + reduceStar + " " + exception);
                    }
                }
            }
            return defaultIfEmpty;
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
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    RTPBot.writeDebugLine("1 != outerXML=" + outerXML);
                }
                var temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    var li = (LineInfoElementImpl)temp;
                    return (LineInfoElementImpl)temp; //.FirstChild;}
                }
                return getNode("<node>" + outerXML + "</node>");
                //return (LineInfoElement)temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                RTPBot.writeDebugLine("outerXML=" + outerXML);
                throw;
            }
        }

        public static XmlNode getNode (string outerXML, XmlNode templateNode) {
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
                var de = doc.DocumentElement;
                //doc.IsReadOnly = false;
                if (doc.ChildNodes.Count == 0)
                {
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    //  return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    RTPBot.writeDebugLine("1 != outerXML=" + outerXML);
                }
                var temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    LineInfoElementImpl li = (LineInfoElementImpl)temp;
                    li.SetParentFromNode(templateNode);
                    return (LineInfoElementImpl)temp; //.FirstChild;}
                }
                return (LineInfoElementImpl)temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                RTPBot.writeDebugLine("ERROR outerXML='" + outerXML + "'\n" + exception + "\n" +
                                      LocationInfo(templateNode));
                throw;
            }
        }
    }
}