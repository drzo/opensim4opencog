using System.Collections.Generic;
using System.IO;
using System.Xml;

namespace MushDLR223.Utilities
{
    public class RenderOptions
    {
        public List<string> flatten;
        public List<string> skip;
        public RenderOptions()
        {            
        }
        public RenderOptions(RenderOptions initial)
        {
            flatten = new List<string>(initial.flatten);
            skip = new List<string>(initial.skip);
        }
        public RenderOptions(ICollection<string> flattern, ICollection<string> skip)
        {
            this.flatten = new List<string>(flattern);
            this.skip = new List<string>(skip);
        }
        public bool FlattenChildren(string nodeName)
        {
            return flatten.Contains(nodeName);
        }

        public bool SkipNode(string nodeName)
        {
            if (skip.Contains(nodeName)) return true;
            return false;
        }

        public string CleanText(string text)
        {
            return text;
        }
    }

    public class XmlNoNamespaceWriter : XmlTextWriter
    {
        private bool skipAttribute = false;

        public XmlNoNamespaceWriter(TextWriter writer)
            : base(writer)
        {
        }

        public override void WriteStartElement(string prefix, string localName, string ns)
        {
            base.WriteStartElement(null, localName, null);
        }


        public override void WriteStartAttribute(string prefix, string localName, string ns)
        {
            //If the prefix or localname are "xmlns", don't write it.
            if (prefix.CompareTo("xmlns") == 0 || localName.CompareTo("xmlns") == 0)
            {
                skipAttribute = true;
            }
            else
            {
                base.WriteStartAttribute(null, localName, null);
            }
        }

        public override void WriteString(string text)
        {
            //If we are writing an attribute, the text for the xmlns
            //or xmlns:prefix declaration would occur here.  Skip
            //it if this is the case.
            if (!skipAttribute)
            {
                base.WriteString(text);
            }
        }

        public override void WriteEndAttribute()
        {
            //If we skipped the WriteStartAttribute call, we have to
            //skip the WriteEndAttribute call as well or else the XmlWriter
            //will have an invalid state.
            if (!skipAttribute)
            {
                base.WriteEndAttribute();
            }
            //reset the boolean for the next attribute.
            skipAttribute = false;
        }


        public override void WriteQualifiedName(string localName, string ns)
        {
            //Always write the qualified name using only the
            //localname.
            base.WriteQualifiedName(localName, null);
        }
    }
}