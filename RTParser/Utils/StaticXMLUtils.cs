using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Xml;

namespace RTParser.Utils
{
    public class StaticXMLUtil : TextPatternUtils
    {
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

        public static bool IsHtmlTag(string name)
        {
            return " html head body font pre p div br dd td th tr table frame frameset &ltl &gt; input option select  "
                .Contains(" " + name.ToLower() + " ");
        }

        public static int FromInsideLoaderContext(XmlNode currentNode, Request request, SubQuery query, Func<int> doit)
        {
            int total = 0;
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            RTPBot RProcessor = request.TargetBot;
            AIMLLoader prev = RProcessor.Loader;
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
                    LineInfoElementImpl xmlNode0 = node as LineInfoElementImpl;
                    int idx = xmlNode0.IndexInBaseParent;
                    if (idx >= 0)
                    {
                        LineInfoElementImpl parentCopy = (LineInfoElementImpl) parentNode.CloneNode(true);
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

        public static LineInfoElementImpl CopyNode(string newName, XmlNode node, bool copyParent)
        {
            XmlDocument od = node.OwnerDocument;
            LineInfoElementImpl newnode =
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

        public static string GetAttribValue(XmlNode templateNode, string attribName, string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, null);
        }

        public static bool TryParseBool(XmlNode templateNode, string attribName, out bool tf)
        {
            return Unifiable.TryParseBool(GetAttribValue(templateNode, attribName, null), out tf);
        }

        public static Unifiable GetAttribValue(XmlNode node, string attribName, Func<string> defaultIfEmpty, SubQuery sq)
        {
            string realName;
            return GetAttribValue(node, attribName, out realName, defaultIfEmpty, sq);
        }

        public static Unifiable GetAttribValue(XmlNode node, string attribName, out string realName,
                                               Func<string> defaultIfEmpty, SubQuery sq)
        {
            realName = null;
            if (node == null) return defaultIfEmpty();
            bool found = false;
            Unifiable u = Unifiable.NULL;
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
                        Unifiable r = StaticAIMLUtils.ReduceStar(attrib.Value, sq, sq);
                        if (!Unifiable.IsNullOrEmpty(r)) return r;
                        if (Unifiable.IsNull(r)) continue;
                        u = r;
                    }
                }
            }
            if (found) return u;
            return defaultIfEmpty();
        }

        public static double GetAttribValue(XmlNode node, string attribName, double defaultIfEmpty, SubQuery sq)
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
            StringReader sr = new StringReader(outerXML);
            try
            {
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("From " + outerXML, false);
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
                XmlNode temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    LineInfoElementImpl li = (LineInfoElementImpl) temp;
                    return temp; //.FirstChild;}
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

        public static XmlNode getNode(string outerXML, XmlNode templateNode)
        {
            StringReader sr = new StringReader(outerXML);
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
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    //  return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    RTPBot.writeDebugLine("1 != outerXML=" + outerXML);
                }
                XmlNode temp = doc.FirstChild;
                if (temp is LineInfoElementImpl)
                {
                    LineInfoElementImpl li = (LineInfoElementImpl) temp;
                    li.SetParentFromNode(templateNode);
                    return temp; //.FirstChild;}
                }
                return temp; //.FirstChild;}
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