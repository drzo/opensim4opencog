using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.XPath;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;
//using LineInfoElement = System.Xml.XmlNode;
using LineInfoElement = RTParser.Utils.LineInfoElementImpl;
using StringAppendableUnifiable = RTParser.StringAppendableUnifiableImpl;
//using StringAppendableUnifiable = System.Text.StringBuilder;

namespace RTParser.Utils
{
    abstract public partial class AIMLTagHandler
    {
        protected void DebugCheck(int i)
        {
            if (!request.IsTraced) return;
            if (request.DebugLevel < i)
            {
                RTPBot.Breakpoint("Level " + request.DebugLevel + "<" + i);
            }
        }

        public override string ToString()
        {
            return LineNumberTextInfo();
        }

        public string LineNumberTextInfo()
        {
            string s = LineTextInfo() + " " + LineNumberInfo();
            if (!s.Contains(initialString))
            {
                return "-WAS- '" + initialString + "' -NOW- " + s;
            }
            return s;
        }

        public string LineTextInfo()
        {
            string s = templateNode.OuterXml.Trim();
            if (String.IsNullOrEmpty(s))
            {
                var li = templateNode;
                s = s + " " + li.OwnerDocument.ToString();
                if (Parent != null && Parent != this)
                {
                    s = s + " " + Parent.LineTextInfo();
                }
                else
                {
                    return s;
                }

            }
            return s;
        }

        public string LineNumberInfo()
        {
            string s = "<!--";
            if (templateNode is IXmlLineInfo)
            {
                IXmlLineInfo li = (IXmlLineInfo)templateNode;
                if (li.LineNumber == 0)
                {
                    s = s + " " + templateNode.OwnerDocument.ToString();
                    if (Parent != null && Parent != this)
                    {
                        s = s + " " + Parent.LineNumberInfo();
                    }
                    else
                    {
                        s = s + " (" + li.LineNumber + ":" + li.LinePosition + ")";
                    }
                }
                else
                {
                    s = s + " (" + templateNode.OwnerDocument.ToString() + ":line " + li.LineNumber + "," + li.LinePosition + ") ";
                }
            }
            return s + "-->";
        }

        public string DocumentInfo()
        {
            string s = null;
            if (templateNode != null)
            {
                var li = templateNode;
                s = "" + li.OwnerDocument;
                if (!string.IsNullOrEmpty(s)) return s;
                if (Parent != null && Parent != this)
                {
                    s = DocumentInfo();
                    if (!string.IsNullOrEmpty(s)) return s;
                }
            }
            return s;
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
            if (templateNode is IXmlLineInfo) return true;
            if (Parent != null && Parent != this) return Parent.HasLineInfo();
            return false;
        }

        /// <summary>
        /// Gets the current line number.
        /// </summary>
        /// <returns>
        /// The current line number or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LineNumber
        {
            get
            {
                IXmlLineInfo ix = (templateNode as IXmlLineInfo) ?? Parent ?? this;
                if (ix != this) return ix.LineNumber;
                //throw new NotImplementedException();
                return 0;
            }
        }

        /// <summary>
        /// Gets the current line position.
        /// </summary>
        /// <returns>
        /// The current line position or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LinePosition
        {
            get
            {
                IXmlLineInfo ix = (templateNode as IXmlLineInfo) ?? Parent ?? this;
                if (ix != this) return ix.LinePosition;
                return 0;
            }
        }
        #endregion

        protected void writeToLogWarn(string unifiable, params object[] objs)
        {
            writeToLog("WARNING: " + unifiable, objs);
        }

        public virtual void writeToLog(string unifiable, params object[] objs)
        {
            if (unifiable.ToUpper().StartsWith("ERROR"))
            {
                writeToLogWarn("BAD " + unifiable, objs);
                return;
            }
            this.Proc.writeToLog("AIMLTRACE: " + unifiable + " in " + GetType().Name + "  " + LineNumberTextInfo(), objs);
        }
    }
}