using System;
using System.Xml;
using MushDLR223.Utilities;

namespace AltAIMLbot.Utils
{
    [Serializable]
    public abstract class GraphLinkInfo : IXmlLineInfo, IHasFilename
    {
        public static bool NoInfo;
        public static bool HoldXMLNode = false;
        protected GraphLinkInfo(XmlNode template)
        {
            if (HoldXMLNode) srcNode = template;

            if (NoInfo)
            {
                throw new InvalidOperationException("now Inof");
            }
        }

        public string InnerXml
        {
            get
            {
                if (!XmlDocumentLineInfo.SkipXmlns && HoldXMLNode && this.srcNode.Attributes != null)
                    this.srcNode.Attributes.RemoveNamedItem("xmlns");
                return srcNode.InnerXml;
            }
        }

        public string OuterXml
        {
            get
            {
                if (!XmlDocumentLineInfo.SkipXmlns && HoldXMLNode && this.srcNode.Attributes != null)
                    this.srcNode.Attributes.RemoveNamedItem("xmlns");
                return srcNode.OuterXml;
            }
        }

        [NonSerialized]
        protected object _srcNode;
        [NonSerialized]
        protected string _srcNodeString;

        public static XmlNode CheckXml(XmlNode xmlNode)
        {
            if (xmlNode != null) return xmlNode;
            return xmlNode;
        }

        #region Implementation of IXmlLineInfo

        /// <summary>
        /// Gets a value indicating whether the class can return line information.
        /// </summary>
        /// <returns>
        /// true if <see cref="P:System.Xml.IXmlLineInfo.LineNumber"/> and <see cref="P:System.Xml.IXmlLineInfo.LinePosition"/> can be provided; otherwise, false.
        /// </returns>
        public bool HasLineInfo()
        {
            return LineNumber != 0 || LinePosition != 0;
        }

        /// <summary>
        /// Gets the current line number.
        /// </summary>
        /// <returns>
        /// The current line number or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LineNumber { get; set; }

        /// <summary>
        /// Gets the current line position.
        /// </summary>
        /// <returns>
        /// The current line position or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LinePosition { get; set; }

        #endregion

        private Unifiable _filename;
        public string Filename
        {
            get { return _filename; }
            set { _filename = value; }
        }

        internal XmlNode srcNode
        {
            get
            {
                XmlNode value0 = _srcNode as XmlNode;
                if (value0 != null) return value0;
                if (_srcNodeString == null)
                {
                    _srcNodeString = MakeFreshXML();
                }
                if (_srcNodeString != null)
                {
                    value0 = StaticAIMLUtils.getNode(_srcNodeString);
                    StaticXMLUtils.SetLineInfo(value0, this, Filename);
                    if (HoldXMLNode) _srcNode = value0;
                    return value0;
                }
                return null;
            }
            set
            {
                if (value == null)
                {
                    _srcNode = null;
                    return;
                }
                if (XmlDocumentLineInfo.SkipXmlns && HoldXMLNode && value.Attributes != null)
                    value.Attributes.RemoveNamedItem("xmlns");
                IXmlLineInfo lineinfo = value as IXmlLineInfo;
                if (lineinfo != null)
                {
                    if (lineinfo.HasLineInfo())
                    {
                        LineNumber = lineinfo.LineNumber;
                        LinePosition = lineinfo.LinePosition;
                    }
                    Filename = StaticXMLUtils.FileNameOfXmlNode(value);
                }
                if (HoldXMLNode || true)
                    _srcNode = value;
                value = CheckXml(StaticXMLUtils.FindNodeOrHigher("template", CheckXml(value), value));
                _srcNodeString = value.OuterXml;
            }
        }

        public abstract string MakeFreshXML();

        abstract public override string ToString();
        //abstract public Unifiable FullPath { get; set; }

    }
}