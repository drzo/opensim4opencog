using System;
using System.IO;
using System.Xml;
using System.Xml.XPath;

namespace MushDLR223.Utilities
{
    public class XmlTextLineInfo : XmlText, XmlSourceLineInfo
    {
        public void SetOwnerDocument(XmlDocumentLineInfo newDoc)
        {
            var prev = OwnerDocument as XmlDocumentLineInfo;
            if (prev != newDoc)
            {
                _docLineInfo = newDoc;
            }
        }

        private XmlDocumentLineInfo _docLineInfo
        {
            get { return null; }
            // ReSharper disable ValueParameterNotUsed
            set { }
            // ReSharper restore ValueParameterNotUsed
        }
        public string outValueReplaced = null;
        public XmlSourceLineInfo lParent
        {
            get { return base.ParentNode as XmlSourceLineInfo; }
            set { throw new NotImplementedException(); }
        }
        public bool protect = false;

        public XmlTextLineInfo(string text, XmlDocumentLineInfo info)
            : base(XmlDocumentLineInfo.Intern(text), info.FileDoc)
        {
            _docLineInfo = info;
        }

        internal XmlDocumentLineInfo docLineInfo
        {
            get { return _docLineInfo ?? OwnerDocument as XmlDocumentLineInfo; }
            set { _docLineInfo = value; }
        }

        public override XmlDocument OwnerDocument
        {
            get { return XmlDocumentLineInfo.DefaultDoc ?? _docLineInfo ?? base.OwnerDocument; }
        }

        public XmlNode CloneOf { get; set; }

        public override string InnerText
        {
            get
            {
                return InnerXml;
                return base.InnerText;
            }
            set
            {
                if (protect)
                {
                    writeToLog("WARNING: InnerText Should not be changed to \"" + value + "\"");
                }
                InnerXml = value;
               // base.InnerText = value;
            }
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
                    var ixml = base.InnerText;
                    var inxml = base.InnerXml;
                    if (inxml != ixml)
                    {
                        writeToLog("InnerText " + ixml + " " + inxml);
                    }
                    if (StaticXMLUtils.IsValueSetter(ixml))
                    {
                        writeToLog("IsValueSetter?! " + ixml);
                      //  ixml = /*StaticXMLUtils.ValueText*/(ixml);
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

        private void writeToLog(string s)
        {
        }

        private bool SetNewValue(string value)
        {
            var ir = value;
            bool wasSetter = false;
            if (StaticXMLUtils.IsValueSetter(value))
            {
                ir = /*StaticXMLUtils.ValueText*/(value);
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
                    var ir = /*StaticXMLUtils.ValueText*/(outValueReplaced);
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
                    if (base.ParentNode == null)
                    {
                        return false;
                    }
                    if (OwnerDocument != null) if (OwnerDocument.IsReadOnly) return XmlDocumentLineInfo._whenReadOnly;
                    return protect;
                }
                return XmlDocumentLineInfo._whenReadOnly;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent as XmlNode;
            }
        }

        #region XmlSourceLineInfo Members

        public bool ReadOnly { get; set; }

        public void SetLineInfo(int linenum, int linepos, string filename)
        {
            if (lParent != null)
            {
                lParent.SetLineInfo(linenum, linepos, filename);
            }
        }

        public int LineNumber
        {
            get { return StaticXMLUtils.ToLineInfo(this).LineNumber; }
        }

        public int LinePosition
        {
            get { return StaticXMLUtils.ToLineInfo(this).LinePosition; }
        }

        public bool HasLineInfo()
        {
            return false;
        }

        public void SetPos(long position)
        {
            if (lParent != null)
            {
                lParent.SetPos(position);
            }
        }

        public void SetParentFromNode(XmlNode xmlNode)
        {
            XmlNode pn = xmlNode.ParentNode;
            if (pn is LineInfoElementImpl)
            {
                lParent = (LineInfoElementImpl)pn;
            }
            if (!(xmlNode is LineInfoElementImpl))
            {
                xmlNode = lParent as XmlNode;
            }
            if (xmlNode is LineInfoElementImpl)
            {
                LineInfoElementImpl lie = (LineInfoElementImpl)xmlNode;
            }
        }

        #endregion

        public override XmlNode CloneNode(bool deep)
        {
            XmlTextLineInfo v = new XmlTextLineInfo(base.Data, (XmlDocumentLineInfo) OwnerDocument);
            v.SetLineInfo(LineNumber, LinePosition, Filename);
            v.ReadOnly = ReadOnly;
            v.CloneOf = CloneOf ?? this;
            return v;
        }

        public string Filename
        {
            get { return StaticXMLUtils.FileNameOfXmlNode(this); }
            set { throw new NotImplementedException(); }
        }

        public override string ToString()
        {
            if (docLineInfo == null) return StaticXMLUtils.TextAndSourceInfo(this);
            return docLineInfo.TextAndSourceInfo(this);
        }
    }
}