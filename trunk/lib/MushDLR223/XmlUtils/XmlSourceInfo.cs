#undef OUTXML_CACHE
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.XPath;

namespace MushDLR223.Utilities
{
    public interface XmlSourceLineInfo : IXmlLineInfo, XmlSourceInfo
    {
        void SetLineInfo(int number, int position);
        void SetPos(long pos);
        void SetParentFromNode(XmlNode xmlNode);
    }

    public interface XmlSourceInfo
    {
        bool ReadOnly { get; set; }
        void SetOwnerDocument(XmlDocumentLineInfo elseway);
        string ToString();
        int GetHashCode();
    }

    public class LineInfoElementImpl : XmlElement, XmlSourceLineInfo
    {
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
        public void SetOwnerDocument(XmlDocumentLineInfo newDoc)
        {
            var prev = OwnerDocument as XmlDocumentLineInfo;
            if (prev != newDoc)
            {
                _docLineInfo = newDoc;
            }
            foreach (var v in ChildNodes)
            {
                XmlDocumentLineInfo.DropDocument(v as XmlSourceLineInfo, newDoc);
            }
        }

        private XmlDocumentLineInfo _docLineInfo;
        /*  {
              get { return null; }
              // ReSharper disable ValueParameterNotUsed
              set { }
              // ReSharper restore ValueParameterNotUsed
          }*/
        public string outValueReplaced = null;
        public XmlSourceLineInfo lParent;
        public bool protect = false;
        public IXmlLineInfo LineNumData
        {
            get
            {
                if (lineNumData != null) return lineNumData;
                return lParent;
            }
        }
        private IXmlLineInfo lineNumData;

        internal LineInfoElementImpl(string prefix, string localname, string nsURI, XmlDocumentLineInfo doc)
            : base(XmlDocumentLineInfo.Intern(prefix), XmlDocumentLineInfo.Intern(localname), XmlDocumentLineInfo.Intern(nsURI), doc.FileDoc)
        {
            var lvar = doc as XmlDocumentLineInfo;
            if (lvar != null)
            {
                _docLineInfo = lvar;
                lvar.CheckNode(this);
            }
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
        }

        internal XmlDocumentLineInfo docLineInfo
        {
            get { return _docLineInfo ?? OwnerDocument as XmlDocumentLineInfo; }
            set { _docLineInfo = value; }
        }

        public override XmlDocument OwnerDocument
        {
            get
            {
                return XmlDocumentLineInfo.DefaultDoc ?? _docLineInfo ?? base.OwnerDocument;
            }
        }

        public int IndexInParent
        {
            get
            {
                XmlNode sib = (XmlNode)lParent;
                if (sib == null)
                {
                    return IndexInBaseParent;
                }
                int idx = 0;
                sib = sib.FirstChild;
                while (sib != null)
                {
                    if (ReferenceEquals(sib, this)) return idx;
                    sib = sib.NextSibling;
                    idx++;
                }
                return IndexInBaseParent;
            }
        }


        public int IndexInBaseParent
        {
            get { return IndexIn(base.ParentNode); }
        }

        public XmlNode CloneOf { get; set; }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return (XmlNode)lParent;
            }
        }

        public virtual XmlNode ParentNodeBase
        {
            get { return base.ParentNode; }
        }

        public override string InnerXml
        {
            get
            {
                try
                {
                    if (outValueReplaced != null)
                    {
                        var vv = StaticXMLUtils.XmlValueSettable(outValueReplaced);
                        return vv;
                    }
                    var ixml = base.InnerXml;
                    if (StaticXMLUtils.IsValueSetter(ixml))
                    {
                        writeToLog("IsValueSetter?! " + ixml);
                        ixml = StaticXMLUtils.ValueText(ixml);
                    }
                    return ixml;
                }
                catch (StackOverflowException exception)
                {
                    string s = exception.ToString();
                    return "<!-- FAILURE " + s + " ==>";
                }
                catch (Exception exception)
                {
                    string s = exception.ToString();
                    return "<!-- FAILURE " + s + " ==>";
                }
            }
            set
            {
                //if (base.InnerXml == value) return;
                if (SetNewValue(value))
                {
                    // base.InnerXml = value;
                    return;
                }
                else
                {
                    return;
                }
                outValueReplaced = StaticXMLUtils.ValueText(value);
                base.InnerXml = value;
            }
        }

        private bool SetNewValue(string value)
        {
            var ir = value;
            bool wasSetter = false;
            if (StaticXMLUtils.IsValueSetter(value))
            {
                ir = StaticXMLUtils.ValueText(value);
                wasSetter = true;
            }
            if (ir.Length == 0)
            {
                if (LocalName != "think")
                {
                    writeToLog("ERROR: SetNewValue Not using " + value);
                    return false;
                }
                outValueReplaced = "";
                return true;
            }
            else
            {
                if (protect)
                {
                    if (value.Contains("<") || value.Contains("+-"))
                    {
                        writeToLog("ERROR: InnerXml Should not be changed to \"" + value + "\"");
                    }
                    else
                    {
                        writeToLog("WARNING: InnerXml Should not be changed to \"" + value + "\"");
                    }
                }
                if (wasSetter)
                {
                    outValueReplaced = ir;
                }
                return true;
            }
        }

#if OUTXML_CACHE
        private string outerXMLCache;
#endif
        public override string OuterXml
        {
            get
            {
#if OUTXML_CACHE
                if (outerXMLCache != null)
                {
                    return outerXMLCache;
                }
                outerXMLCache = base.OuterXml;
#else
                var outerXMLCache = base.OuterXml;
#endif
                if (outValueReplaced != null)
                {
                    var ir = StaticXMLUtils.ValueText(outValueReplaced);
                    if (ir.Length == 0)
                    {
                        writeToLog("Not using " + outValueReplaced);
                    }
                    else
                    {
                        writeToLog("Not using " + outValueReplaced);
                        //return ir;
                    }
                }
                if (true) return outerXMLCache;
                bool RemoveXmlns = true;
                if (docLineInfo != null)
                {
                    RemoveXmlns = docLineInfo.RemoveXmlns;
                }
                if (RemoveXmlns && outerXMLCache.Contains("xmlns"))
                {
                    StringWriter sw = new StringWriter();
                    var dw = new XmlNoNamespaceWriter(sw);
                    XPathNavigator nav = CreateNavigator();
                    dw.WriteNode(nav, false);
                    outerXMLCache = sw.ToString();
                }
                return outerXMLCache;
            }
        }

        public override string InnerText
        {
            get
            {
                return InnerXml;
                //return base.InnerText;
            }
            set
            {
                if (protect)
                {
                    writeToLog("WARNING: InnerText Should not be changed to \"" + value + "\"");
                }
                InnerXml = value;
            }
        }

        public override bool IsReadOnly
        {
            get
            {
                if (!base.IsReadOnly)
                {
                    if (!protect) return false;

                    if (CloneOf == null)
                    {
                        return protect || XmlDocumentLineInfo._whenReadOnly;
                    }
                    if (OwnerDocument != null) if (OwnerDocument.IsReadOnly) return XmlDocumentLineInfo._whenReadOnly;
                    return protect;
                }
                return XmlDocumentLineInfo._whenReadOnly;
            }
        }

        public override string LocalName
        {
            get
            {
                CheckOutVlaueReplaced();
                return base.LocalName;
            }
        }

        public override string Name
        {
            get
            {
                CheckOutVlaueReplaced();
                return base.Name;
            }
        }

        public override XmlNodeType NodeType
        {
            get
            {
                CheckOutVlaueReplaced();
                return base.NodeType;
            }
        }

        private void CheckOutVlaueReplaced()
        {
            if (outValueReplaced == null) return;
        }

        #region XmlSourceLineInfo Members

        public void SetLineInfo(int linenum, int linepos)
        {
            if (linenum < LineNumber)
            {
                writeToLog("Line number too small");
                return;
            }
            if (linenum != 0 || linepos != 0)
            {
                if (lineNumData == null)
                {
                    lineNumData = new LineInfoData(linenum, linepos);
                }
                else
                {
                    if (lineNumData is LineInfoData)
                    {
                        var lid = lineNumData as LineInfoData;
                        lid.LineNumber = linenum;
                        lid.LinePosition = linepos;
                    }
                    else
                    {
                        lineNumData = new LineInfoData(linenum, linepos);
                    }
                }
            }
        }

        public int LineNumber
        {
            get
            {
                if (lineNumData == null) return 0;
                return lineNumData.LineNumber;
            }
        }

        public int LinePosition
        {
            get
            {
                if (lineNumData == null) return 0;
                return lineNumData.LinePosition;
            }
        }

        public bool HasLineInfo()
        {
            return lineNumData is LineInfoData;
        }

        public void SetPos(long position)
        {

        }

        public bool ReadOnly
        {
            get
            {
                if (base.IsReadOnly)
                {
                    if (!protect)
                    {
                        protect = IsReadOnly;
                    }
                }
                return protect;
            }
            set
            {
                bool vitalChange = false;
                if (protect != IsReadOnly)
                {
                    vitalChange = true;
                }
                if (protect != value)
                {
                    protect = value;
                    if (!vitalChange)
                    {
                        vitalChange = true;
                    }
                }
                foreach (XmlNode node in ChildNodes)
                {
                    if (node is XmlSourceInfo)
                    {
                        ((XmlSourceInfo)node).ReadOnly = value;
                    }
                    else
                    {
                        if (!node.IsReadOnly && !value)
                        {
                            writeToLog("Non Readonly Child is not a AIMLXmlInfo: " + node);
                        }
                        if (vitalChange)
                        {
                            writeToLog("ERROR: Child is not a AIMLXmlInfo: " + node);
                        }
                        else
                        {
                            writeToLog("WARNING: Child is not a AIMLXmlInfo: " + node);
                        }
                    }
                }
                foreach (object node in Attributes)
                {
                    if (node is XmlSourceInfo)
                    {
                        ((XmlSourceInfo)node).ReadOnly = value;
                    }
                    else
                    {
                        writeToLog("Attribute is not a AIMLXmlInfo: " + node);
                    }
                }
            }
        }

        public void SetParentFromNode(XmlNode xmlNode)
        {
            XmlNode pn = xmlNode.ParentNode;
            if (pn is LineInfoElementImpl)
            {
                lParent = (LineInfoElementImpl)pn;
                XmlDocumentLineInfo.SuggestLineNo(lParent, this);
            }
            if (!(xmlNode is IXmlLineInfo))
            {
                xmlNode = (XmlNode)lParent;
            }
            if (xmlNode is LineInfoElementImpl)
            {
                LineInfoElementImpl lie = (LineInfoElementImpl)xmlNode;
                lineNumData = lie;
                XmlDocumentLineInfo.SuggestLineNo(lie, this);
            }
        }

        #endregion

        public override void Normalize()
        {
            if (OwnerDocument.IsReadOnly) OwnerDocument.Normalize();
            else
            {
#if OUTXML_CACHE
                outerXMLCache = null;
#endif
                base.Normalize();
            }
        }

        public override string ToString()
        {
            try
            {
                return DebugString;
            }
            catch (Exception e)
            {
                DLRConsole.SYSTEM_ERR_WRITELINE("XML TOSTRING PROBLEM " + e);
                throw;
            }
        }

        public string DebugString
        {
            get
            {
                string was = base.OuterXml;
                if (outValueReplaced != null)
                {
                    was = outValueReplaced + " <!-- WAS = " + was + "-->";
                }
                return was + " " + StaticXMLUtils.LocationEscapedInfo(this);
            }
        }


        internal void SetParent(LineInfoElementImpl pn)
        {
            lParent = pn;
        }


        public int IndexIn(XmlNode parent)
        {
            if (parent == null) return -2;
            int idx = 0;
            XmlNode sib = parent.FirstChild;
            while (sib != null)
            {
                if (ReferenceEquals(sib, this)) return idx;
                sib = sib.NextSibling;
                idx++;
            }
            return -1;
        }

        public override XmlNode CloneNode(bool deep)
        {
            XmlNode highestClone = CloneOf ?? this; ;
            var od = highestClone.OwnerDocument as XmlDocumentLineInfo;
            //od.Normalize();
            LineInfoElementImpl newnode = new LineInfoElementImpl(Prefix, LocalName, NamespaceURI, od);
            newnode.CloneOf = highestClone;
#if OUTXML_CACHE      
            newnode.outerXMLCache = outerXMLCache;
#endif
            newnode.SetLineInfo(LineNumber, LinePosition);
            if (deep)
            {
                bool newnodeWas = newnode.protect;
                newnode.protect = false;
                if (HasChildNodes)
                    foreach (XmlNode a in ChildNodes)
                    {
                        try
                        {
                            XmlNode a2 = a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            XmlSourceInfo a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.AppendChild(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                XmlAttributeCollection ats = Attributes;
                if (ats != null)
                    foreach (XmlAttribute a in ats)
                    {
                        try
                        {
                            if (XmlDocumentLineInfo.SkipXmlns && a.Name == "xmlns") continue;
                            XmlAttribute a2 = (XmlAttribute)a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            XmlSourceInfo a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.Attributes.Append(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            newnode.writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                newnode.protect = newnodeWas;
            }
            else
            {
                XmlAttributeCollection ats = Attributes;
                if (ats != null)
                    foreach (XmlAttribute a in ats)
                    {
                        try
                        {
                            if (XmlDocumentLineInfo.SkipXmlns && a.Name == "xmlns") continue;
                            XmlAttribute a2 = (XmlAttribute)a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly && false;
                            XmlSourceInfo a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.Attributes.Append(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            newnode.writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                newnode.protect = false;
            }
            return newnode;
        }

        public override XmlNode RemoveChild(XmlNode newChild)
        {
            bool before = XmlDocumentLineInfo._whenReadOnly;
            try
            {
#if OUTXML_CACHE
                outerXMLCache = null;
#endif
                if (ReadOnly)
                {
                    writeToLog("ERROR RemoveChild on ReadOnly ");
                    ReadOnly = false;
                }
                XmlDocumentLineInfo._whenReadOnly = false;
                return base.RemoveChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.RemoveChild " + e);
                return null;
            }
            finally
            {
                XmlDocumentLineInfo._whenReadOnly = before;
            }
        }

        public override XmlNode AppendChild(XmlNode newChild)
        {
            try
            {
#if OUTXML_CACHE
                outerXMLCache = null;
#endif
                if (newChild.OwnerDocument != OwnerDocument)
                {
                    if (newChild.NodeType == XmlNodeType.Text)
                    {
                        newChild = OwnerDocument.CreateTextNode(newChild.InnerText);
                    }
                }
                return base.AppendChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.AppendChild " + e);
                return null;
            }
        }

        public override void RemoveAll()
        {
            try
            {
                base.RemoveAll();
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.RemoveAll " + e);
                return;
            }
        }
        public override XmlNode NextSibling
        {
            get
            {
                try
                {
                    return base.NextSibling;
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: newnode.NextSibling " + e);
                    throw;
                }
            }
        }
        public override XmlNode PrependChild(XmlNode newChild)
        {
            try
            {
                return base.PrependChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.PrependChild " + e + " " + newChild);
                throw;
            }
        }

        public static IXmlLineInfo ToLineInfoElement(XmlNode pattern)
        {
            //return pattern as IXmlLineInfo
            if (pattern == null) return null;
            if (pattern is IXmlLineInfo)
            {
                return (IXmlLineInfo)pattern;
            }
            return null; // CopyNode(pattern, true);
        }

        internal void writeToLog(string s)
        {
            XmlDocumentLineInfo.DebugWriteLine(s + " on XML node: '" + this + "'");
        }

        public static void unsetReadonly(LineInfoElementImpl node)
        {
            if (node.ReadOnly)
            {
                node.ReadOnly = false;
            }
        }

        public static void unsetReadonly(XmlSourceInfo node)
        {
            if (node.ReadOnly)
            {
                node.ReadOnly = false;
            }
        }

        public static void unsetReadonly(XmlNode node)
        {
            XmlSourceLineInfo lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                unsetReadonly(lie);
            }
        }

        public static void notReadonly(XmlNode node)
        {
            XmlSourceLineInfo lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                unsetReadonly(lie);
            }
        }

        public static void chopParent(XmlNode chilz)
        {
            XmlNode p = chilz.ParentNode;
            if (p == null) return;
            LineInfoElementImpl lie = p as LineInfoElementImpl;
            if (!p.IsReadOnly || lie == null)
            {
                p.RemoveChild(chilz);
                return;
            }
            bool liewas = lie.ReadOnly;
            try
            {
                lie.ReadOnly = false;
                lie.RemoveChild(chilz);
            }
            finally
            {
                lie.ReadOnly = liewas;
            }

        }

        public static XmlNode SetReadOnly(XmlNode node)
        {
            XmlSourceLineInfo lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                lie.ReadOnly = true;
            }
            return node;
        }

        public static void SetParentFromNode(XmlNode newLineInfoPattern, XmlNode patternNode)
        {
            ((LineInfoElementImpl)newLineInfoPattern).SetParentFromNode(patternNode);
        }

        public override void WriteContentTo(XmlWriter w)
        {
            base.WriteContentTo(w);
        }

        public override void WriteTo(XmlWriter w)
        {
            base.WriteTo(w);
        }
    }

    public class LineInfoData : IXmlLineInfo
    {
        public override string ToString()
        {
            return LineNumber + ":" + LinePosition;
        }
        public LineInfoData(int linenum, int linepos)
        {
            LineNumber = linenum;
            LinePosition = linepos;
        }
        public bool HasLineInfo()
        {
            return true;
        }
        public int LineNumber { get; set; }
        public int LinePosition { get; set; }
    }

    // End LineInfoElement class.
}