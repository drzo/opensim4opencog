#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#if (!VISUAL_STUDIO)
#undef mswindows
#endif

/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2009 John Pool -- j.pool@ision.nl
                   Contributions 2009 by Lars Iwer -- lars.iwer@inf.tu-dresden.de

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.Text;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Xml;

namespace RTParser.Prolog
{
    public class Node
    {
        private XmlNodeType type;
        private string name;
        private string text; // contains prefix in case of element
        private ArrayList attributes;
        private ArrayList childNodes;

        public Node()
        {
            attributes = new ArrayList();
            childNodes = new ArrayList();
        }

        public Node(string nodeName)
        {
            name = nodeName;
            attributes = new ArrayList();
            childNodes = new ArrayList();
        }

        public XmlNodeType Type { get { return type; } }
        public string TagName { get { return name; } set { name = value; } }
        public string Text { get { return text; } set { text = value; } }
        public ArrayList ChildNodes { get { return childNodes; } }
        public ArrayList Attributes { get { return attributes; } }

        public string AttributeValue(string attributeName)
        {
            foreach (DictionaryEntry de in attributes)
                if ((string)de.Key == attributeName) return (string)de.Value;

            return null;
        }

        public void AddAttribute(string attributeName, string attributeValue)
        {
            attributes.Insert(0, new DictionaryEntry(attributeName, attributeValue));
        }


        /// Conversion of an XML-structure (in a string or in a file) to a Prolog Term

        public static Term XmlToTerm(string s, bool inFile)
        {
            XmlTextReader xrd;

            if (inFile)
                xrd = new XmlTextReader(s);
            else
                xrd = new XmlTextReader(new StringReader(s));

            //xrd.ProhibitDtd = true; // new in the .NET Framework version 2.0
            Node result = new Node();
            result.TagName = "<root>";
            result.type = XmlNodeType.Element;
            try
            {
                result.ToNode(xrd, 0); // first, create an intermediate representation (a Node) containing the XML structure
            }
            finally
            {
                xrd.Close();
            }
            return result.ToTerm(); // Convert the Node to a Prolog Term
        }


        public void ToNode(XmlTextReader reader, int level)
        {
            Node child; // essentially: in the loop, add new nodes to this.childNodes

            while (reader.Read())
            {
                //Console.WriteLine ("Read name={0} value={1} reader.NodeType={2} Level={3}", reader.Name, reader.Value.Trim (), reader.NodeType, level);
                switch (reader.NodeType)
                {
                    case XmlNodeType.Element:
                        // create a new subelement
                        child = new Node();
                        child.type = XmlNodeType.Element;
                        child.name = reader.LocalName;
                        child.text = reader.Prefix;
                        while (reader.MoveToNextAttribute())
                            child.AddAttribute(reader.Name, reader.Value);
                        child.ToNode(reader, level + 1);
                        childNodes.Insert(0, child);
                        break;
                    case XmlNodeType.Attribute:
                        this.AddAttribute(reader.Name, reader.Value);
                        break;
                    case XmlNodeType.EndElement:
                        return;
                    case XmlNodeType.Comment:
                    case XmlNodeType.Text:
                    case XmlNodeType.CDATA:
                        child = new Node();
                        child.type = reader.NodeType;
                        child.Text = reader.Value;
                        childNodes.Insert(0, child);
                        break;
                    case XmlNodeType.ProcessingInstruction:
                        child = new Node();
                        child.type = reader.NodeType;
                        child.name = reader.Name;
                        child.text = reader.Value;
                        childNodes.Insert(0, child);
                        break;
                    case XmlNodeType.XmlDeclaration:
                        while (reader.MoveToNextAttribute())
                            this.AddAttribute(reader.Name, reader.Value);
                        break;
                    case XmlNodeType.Document:
                    case XmlNodeType.DocumentFragment:
                    case XmlNodeType.DocumentType:
                    case XmlNodeType.EndEntity:
                    case XmlNodeType.Entity:
                    case XmlNodeType.EntityReference:
                    case XmlNodeType.None:
                    case XmlNodeType.Notation:
                    case XmlNodeType.SignificantWhitespace:
                    case XmlNodeType.Whitespace:
                        // ignore
                        break;
                    default:
                        //Console.WriteLine ("*** Unhandled reader.NodeType: {0}", reader.NodeType);
                        break;
                }
            }
            return;
        }


        // ToTerm results in term xml( Pre, Element, Post), where Pre and Post are lists with comments
        // and/or processing instructions that come before/after the top-level XML element.
        public Term ToTerm()
        {
            Term pre = Term.NULLLIST;
            Term post = Term.NULLLIST;
            Term t = null;
            Node topEl = null;

            foreach (Node n in childNodes) // array was constructed in reverse order, so top-level XML-element is entry 0.
            {
                switch (n.type)
                {
                    case XmlNodeType.Element:
                        topEl = n;
                        break;
                    case XmlNodeType.Comment:
                        t = new Term("comment", new Term(n.Text, FType.text), OType.noop, 0);
                        if (topEl == null) post = new ListTerm(t, post); else pre = new ListTerm(t, pre);
                        break;
                    case XmlNodeType.ProcessingInstruction:
                        t = new Term("instructions", new Term(Utils.MakeAtomic(n.name)),
                                                        new Term(n.text.Trim(), FType.text), FType.comp, OType.noop, 0);
                        if (topEl == null) post = new ListTerm(t, post); else pre = new ListTerm(t, pre);
                        break;
                }
            }

            Term xmlDecl = Term.NULLLIST;

            foreach (DictionaryEntry de in Attributes) // XML Declaration (acually a PI) was stored in Attributes
            {
                Term pair = new Term(Parser.EQ, new Term((string)de.Key), new Term((string)de.Value, FType.text), FType.comp, OType.xfx, 700);
                xmlDecl = new ListTerm(pair, xmlDecl); // [pre, arg2, ...]
            }

            if (xmlDecl != Term.NULLLIST) // enhance list with XML Declaration
            {
                xmlDecl = new Term("xmldecl", xmlDecl, OType.noop, 0);
                pre = new ListTerm(xmlDecl, pre);
            }

            Term content = ToTermEx(topEl); // Now process top-level Element

            return new Term("xml", new Term[] { pre, content, post }, FType.comp, OType.noop, 0);
        }


        private static Term ToTermEx(Node root)
        {
            Term[] args = new Term[3];

            if (root.Text == null || root.Text == "")
                args[0] = new Term(Utils.MakeAtom(root.TagName));
            else
                args[0] = new Term(":",
                                     new Term(Utils.MakeAtomic(root.Text)),
                                     new Term(Utils.MakeAtomic(root.TagName)),
                                     FType.comp, OType.xfy, 600);
            args[1] = Term.NULLLIST;

            foreach (DictionaryEntry de in root.Attributes) // XML Declaration
            {
                Term pair = new Term(Parser.EQ, new Term((string)de.Key), new Term((string)de.Value, FType.text), FType.comp, OType.xfx, 700);
                args[1] = new ListTerm(pair, args[1]);
            }

            args[2] = Term.NULLLIST; // []

            if (root.ChildNodes.Count > 0)
            {
                foreach (Node n in root.ChildNodes)
                {
                    Term e;
                    e = null;
                    switch (n.type)
                    {
                        case XmlNodeType.Element:
                            e = ToTermEx(n);
                            break;
                        case XmlNodeType.Comment:
                            e = new Term("comment", new Term(n.text.Trim(), FType.text), OType.noop, 0);
                            break;
                        case XmlNodeType.Text:
                            e = new Term("text", new Term(n.text.Trim(), FType.text), OType.noop, 0);
                            break;
                        case XmlNodeType.CDATA:
                            e = new Term("cdata", new Term(n.text.Trim(), FType.text), OType.noop, 0);
                            break;
                        case XmlNodeType.ProcessingInstruction:
                            e = new Term("instructions", new Term(Utils.MakeAtomic(n.name)),
                                                          new Term(n.text.Trim(), FType.text), FType.comp, OType.noop, 0);
                            break;
                        case XmlNodeType.SignificantWhitespace:
                        case XmlNodeType.Whitespace:
                            break;
                        default:
                            break;
                    }
                    if (e != null) args[2] = new ListTerm(e, args[2]);
                }
            }
            return new Term("element", args);
        }


        /// Conversion of a Prolog Term to an XML-structure (in a string or in a file)

        public static bool TermToXml(Term settings, Term xmlTerm, ref string fileNameOrXmlString) // xmlTerm = xml( [<pi>?], element (...))
        {
            // get settings
            bool isXChars = true; // not used
            bool isFormat = true;
            bool isRemPrf = false; // not used
            bool isIndent = true;
            bool isDtdChk = false; // not used
            Encoding encoding = Encoding.UTF8;

            while (settings != null && settings != Term.NULLLIST) // traverse ...
            {
                Term e = settings.Arg(0);
                string setting = e.Functor;
                string value; // value of setting
                if (e.Arity == 1) value = e.Arg(0).Functor; else continue;
                switch (setting)
                {
                    case "extended_characters": // Use the extended character entities for XHTML (default true)
                        isXChars = (value == "true");
                        break;
                    case "format": // Strip layouts when no character data appears between elements.
                        isFormat = (value == "true");
                        break;
                    case "remove_attribute_prefixes": // Remove namespace prefixes from attributes when it
                        // is the same as the prefix of the parent element
                        isRemPrf = (value == "true");
                        break;
                    case "indent": // Indent the element content (2 spaces)
                        isIndent = (value == "true");
                        break;
                    case "encoding": // Encoding to appear in XML-declaration
                        //encoding = Encoding.GetEncoding (value); // Strings, such as "utf-8", should work, but don't
                        encoding = Encoding.GetEncoding(Convert.ToInt32(value)); // 65001 = "utf-8"
                        break;
                    case "check_dtd": // Read the referenced DTD
                        isDtdChk = (value == "true");
                        break;
                }
                settings = settings.Arg(1);
            }

            bool result = false;
            XmlTextWriter xwr = null;
            StringWriter sw = new StringWriter();

            try
            {
                Term t0 = xmlTerm.Arg(0);

                if (fileNameOrXmlString == null) // return flat XmlString
                {
                    xwr = new XmlTextWriter(sw);
                    xwr.Formatting = Formatting.None;
                }
                else // write to file
                {
                    xwr = new XmlTextWriter(fileNameOrXmlString, encoding);
                    xwr.Formatting = isFormat ? Formatting.Indented : Formatting.None;
                    xwr.Indentation = 2;
                    xwr.IndentChar = ' '; // default
                    xwr.Namespaces = true;
                    ContentTermToXml(xwr, t0);
                }
                result = ElementTermToXml(xwr, xmlTerm.Arg(1)); // top-level element
            }
            finally
            {
                if (fileNameOrXmlString == null)
                    fileNameOrXmlString = sw.ToString();
                else
                    xwr.Close();
            }
            return result;
        }


        private static bool ContentTermToXml(XmlTextWriter xwr, Term list) // process an element content, i.e. a list
        {
            while (list != Term.NULLLIST) // traverse ...
            {
                Term e = list.Arg(0);
                string type = e.Functor;

                switch (type)
                {
                    case "xmldecl":
                        xwr.WriteStartDocument(true);
                        break;
                    case "element":
                        if (!ElementTermToXml(xwr, e)) return false;
                        break;
                    case "text":
                        xwr.WriteString(e.Arg(0).Functor);
                        break;
                    case "cdata":
                        xwr.WriteCData(e.Arg(0).Functor);
                        break;
                    case "comment":
                        xwr.WriteComment(e.Arg(0).Functor);
                        break;
                    case "instructions":
                        xwr.WriteProcessingInstruction(e.Arg(0).Functor, e.Arg(1).ToString());
                        break;
                    default:
                        PrologIO.Error("ContentTermToXml -- unrecognized type '{0}'", type);
                        break;
                }
                list = list.Arg(1);
            }
            return true;
        }


        private static bool ElementTermToXml(XmlTextWriter xwr, Term e) // process an element( <tag>, <attributes>, <content>)
        {
            if (e.Arity != 3) return false;

            // open tag
            xwr.WriteStartElement(e.Arg(0).ToString());  // writes ns:localName: Arg(0) *may* be ':'(ns, localName)

            // attributes
            Term le = e.Arg(1); // list with attribute-value pairs
            while (le != Term.NULLLIST)
            {
                Term av = le.Arg(0); // Term: attr = value
                xwr.WriteAttributeString(av.Arg(0).Functor, av.Arg(1).Functor);
                le = le.Arg(1);
            }

            // content
            if (!ContentTermToXml(xwr, e.Arg(2))) return false;
            xwr.WriteEndElement();

            return true;
        }


        // ToString ()

        new public string ToString()
        {
            StringBuilder sb = new StringBuilder();
            ToStringEx(this, sb, 0);
            return sb.ToString();
        }


        private static void ToStringEx(Node root, StringBuilder sb, int depth)
        {
            sb.Append(new string(' ', depth) + "<" + root.TagName);

            foreach (DictionaryEntry de in root.Attributes)
                sb.Append(String.Format(@" {0}=""{1}""", (string)de.Key, (string)de.Value));

            sb.Append(">" + root.Text);

            if (root.ChildNodes.Count > 0)
            {
                sb.Append(Environment.NewLine);
                foreach (Node n in root.ChildNodes)
                {
                    switch (n.type)
                    {
                        case XmlNodeType.Element:
                            ToStringEx(n, sb, depth + 2);
                            break;
                        case XmlNodeType.Comment:
                            sb.Append("<!--" + n.text + "-->");
                            break;
                        case XmlNodeType.Text:
                            sb.Append("[" + n.text.Trim() + "]");
                            break;
                        case XmlNodeType.CDATA:
                        case XmlNodeType.ProcessingInstruction:
                        case XmlNodeType.SignificantWhitespace:
                        case XmlNodeType.Whitespace:
                            break;
                        default:
                            break;
                    }
                }
                sb.Append(String.Format("{0}</{1}>{2}", new string(' ', depth), root.TagName, Environment.NewLine));
            }
            else
                sb.Append("</" + root.TagName + ">" + Environment.NewLine);
        }
    }
}
