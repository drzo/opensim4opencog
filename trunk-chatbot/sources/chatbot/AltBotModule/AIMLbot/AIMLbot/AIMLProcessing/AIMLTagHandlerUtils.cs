using System;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using MushDLR223.Utilities;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;

//using StringAppendableUnifiable = System.Text.StringBuilder;

namespace AltAIMLbot.Utils
{
    public abstract partial class AIMLTagHandlerU
    {
        protected void DebugCheck(int i)
        {
            QuerySettingsReadOnly request = (QuerySettingsReadOnly)this.request;
            if (!request.IsTraced) return;
            if (request.DebugLevel < i)
            {
                AltBot.Breakpoint("Level " + request.DebugLevel + "<" + i);
            }
        }

        public override string ToString()
        {
            return LineNumberTextInfo();
        }

        public string LineNumberTextInfo()
        {
            string s = LineTextInfo() + " " + LineNumberInfo();
            string ss = s;
            if (ss.StartsWith(isValueSetStart))
            {
                ss = s + " -FROM- " + templateNode.OuterXml;
            }
            if (!s.Replace(" /", "/").Contains(initialString.Replace(" /", "/")))
            {
                return "-WAS- '" + initialString + "' -NOW- " + ss;
            }
            return ss;
        }

        public string LineTextInfo()
        {
            string s = Trim(templateNode.OuterXml);
            if (String.IsNullOrEmpty(s))
            {
                XmlNode li = templateNode;
                s = s + " " + li.OwnerDocument;
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
                IXmlLineInfo li = (IXmlLineInfo) templateNode;
                if (li.LineNumber == 0)
                {
                    s = s + " " + templateNode.OwnerDocument;
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
                    s = s + " (" + templateNode.OwnerDocument + ":line " + li.LineNumber + "," + li.LinePosition + ") ";
                }
            }
            return s + "-->";
        }

        public string DocumentInfo()
        {
            string s = null;
            if (templateNode != null)
            {
                XmlNode li = templateNode;
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
            this.Proc.writeToLog("AIMLTRACE: " + unifiable + DLRConsole.NoFormatDirectives(" in " + GetType().Name + "  " + LineNumberTextInfo()), objs);
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

        internal static string GetNameOfDict(SubQuery query, string dictName, XmlNode templateNode, out ISettingsDictionary dict)
        {
            string type = ToLower(dictName);
            //ISettingsDictionary udict = query.GetDictionary(type, templateNode, dict);
            while (templateNode != null)
            {
                string type0 = GetAttribValue(templateNode, "type,dict", null);
                if (type0 != null)
                {
                    type = type0;
                    break;
                }
                string uname = GetAttribValue(templateNode, "user", null);
                if (uname != null)
                {
                    type0 = GetNamedType("user", uname);

                    if (type0 != null)
            {
                        type = type0;
                        break;
                    }
                }
                string bname = GetAttribValue(templateNode, "bot", null);
                if (bname != null)
                {
                    type0 = GetNamedType("bot", bname);
                    if (type0 != null)
                    {
                        type = type0;
                        break;
                    }
                }
                dict = query.GetDictionary(templateNode.LocalName);
                if (dict != null)
                {
                    type = dict.NameSpace;
                    break;
                }
                templateNode = templateNode.ParentNode;
            }
            if (type == null) type = dictName;
            bool preferBotOverUser = (type == "bot");

            if (preferBotOverUser)
            {
                dict = query.ResponderPredicates;
            }
            else
            {
                dict = query.GetDictionary(type);
            }
            if (dict == null) dict = query;
            return type ?? dict.NameSpace;
        }
    }
}