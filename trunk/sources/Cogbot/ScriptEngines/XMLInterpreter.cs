using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.IO;
using System.Xml;

namespace textsl.Utilities
{
    namespace XMLInterpreter
    {
        public class XmlReader
        {
            private XmlNode currNode;

            public XmlReader(XmlNode currNode)
            {
                this.currNode = currNode;
            }

            public XmlReader(XmlDocument doc)
                : this(doc.FirstChild)
            {
            }

            public XmlReader(string XMLPath)
            {
                XmlDocument doc = new XmlDocument();
                XmlTextReader reader = new XmlTextReader(XMLPath);

                reader.Read();
                doc.Load(reader);
                this.currNode = doc.FirstChild;
            }

            //Get child value of a node
            public string this[string child]
            {
                get
                {
                    return currNode.SelectSingleNode("child::" + child).InnerText;
                }
            }

            // Get child attribute
            public string this[string child, string attribute]
            {
                get
                {
                    return currNode.SelectSingleNode("child::" + child).Attributes[attribute].Value;
                }
            }

            public bool hasChildValue(string child)
            {
                return (hasChild(child) && (this[child].Length > 0));
            }

            public bool hasChild(string child)
            {
                return (this.currNode.SelectSingleNode("child::" + child) != null);
            }

            // Get Child node collection from a wrapper name
            public XmlReaderCollection getSubNodeCollection(string collectionWrapperChild)
            {
                return new XmlReaderCollection(currNode.SelectSingleNode("child::" + collectionWrapperChild).ChildNodes);
            }

            //Get All Child nodes
            public XmlReaderCollection getAllChildren()
            {
                return new XmlReaderCollection(currNode.ChildNodes);
            }
        }

        [Serializable()]
        public class XmlReaderCollection : CollectionBase
        {
            public XmlReaderCollection()
            {
            }

            public XmlReaderCollection(XmlNodeList nodes)
            {
                foreach (XmlNode node in nodes)
                {
                    this.Add(new XmlReader(node));
                }
            }

            // Returns an XmlReader object from the specified ordinal position within the collection.
            public XmlReader this[int index]
            {
                get
                {
                    return (XmlReader)List[index];
                }
                set
                {
                    List[index] = value;
                }
            }

            // value: The XmlReader object to add to the collection
            // returns: The position within the collection that the XmlReader object was added
            public int Add(XmlReader value)
            {
                return (List.Add(value));
            }


            // Determines the index in the XmlReaderCollection of the XmlReader object specified.
            public int IndexOf(XmlReader value)
            {
                return (List.IndexOf(value));
            }


            // value: The zero based index at which the XmlReader object should be added
            //index: the XmlReader to be added to the XmlReaderCollection.
            public void Insert(int index, XmlReader value)
            {
                List.Insert(index, value);
            }


            // Removes the XmlReader object from the XmlReaderCollection.
            public void Remove(XmlReader value)
            {
                List.Remove(value);
            }


            // Determines if the XmlReaderCollection contains the specified XmlReader object.
            public bool Contains(XmlReader value)
            {
                return (List.Contains(value));
            }
        }
    }
}
