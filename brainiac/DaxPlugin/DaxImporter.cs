using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Net;
using System.Web;
using System.Xml;
using System.Windows.Forms;
using System.Globalization;
using System.IO;
using Brainiac.Design.Attributes;
using Brainiac.Design.Nodes;
using System.Reflection;
using Brainiac.Design.Properties;

using Brainiac.Design.FileManagers;
using Brainiac.Design;


namespace DaxPlugin
{
   public class DaxImporter : FileManagerXML
    {
 		/// <summary>
		/// Creates a new XML file manager to load and save behaviours.
		/// </summary>
		/// <param name="filename">The file we want to load from or save to.</param>
		/// <param name="node">The node we want to save. When loading, use null.</param>
       public DaxImporter(string filename, BehaviorNode node)
           : base(filename, node)
		{
            if (filename.Length > 0)
            {
                if (filename.StartsWith ("http://"))
                {
                    _filename=filename;
                }
                else{
                _filename = Path.GetFullPath(filename);
                }
            }
            _node = node;
        }

       public string capitalize(string txt)
       {
           if ((txt!=null)&&(txt.Length > 1))
           {
               txt = txt.Substring(0, 1).ToUpper() + txt.Substring(1);
           }
           return txt;
       }

       public void importEditNode(XmlNode xml)
       {
           if ((xml.Name.ToLower() == "say") && (xml.ChildNodes.Count==1))
           {
               string inner = xml.InnerXml;
               string outer = xml.OuterXml ;
               if (outer.StartsWith("<say><sapi>"))
               {
                   outer = outer.Replace("<say>", "");
                   outer = outer.Replace("</say>", "");
                   string pattern =@"\<silence (.*?)\>"; 
                   outer = Regex.Replace(outer,pattern,@"</sapi><sapi><silence $1></sapi><sapi>");
                   pattern = @"\<bookmark (.*?)\>";
                   outer = Regex.Replace(outer, pattern, @"</sapi><sapi><bookmark $1></sapi><sapi>");
                   pattern = @"\<sapi\>\</sapi\>";
                   outer = Regex.Replace(outer, pattern, @"");
                   xml.InnerXml = outer;
               }
           }
       }
       /// <summary>
        /// Loads a node from a given XML node.
        /// </summary>
        /// <param name="xml">The XML node we want to create the node from.</param>
        /// <returns>Returns the created node.</returns>
        protected override Node CreateNode(XmlNode xml)
        {
            // get the type of the node and create it
            string clss = "";
            Type t = null;
            Type dt = null;
            string daxClass = "DaxPlugin.Nodes.Dax" +capitalize(xml.Name);
            dt = Plugin.GetType(daxClass);
            if (dt == null)
            {
                bool foundAttr = GetAttribute(xml, "Class", out clss);

                if (foundAttr) t = Plugin.GetType(clss);

                if ((dt != null) && (t == null)) t = dt;
            }
            else
            {
                t = dt;
            }

            if (t == null)
                throw new Exception(string.Format("ExceptionUnknownNodeType :{0} {1}", clss, daxClass));

            Node node = Brainiac.Design.Nodes.Node.Create(t);

            // update the loaded behaviour member
            if (node is BehaviorNode && _loadedBehavior == null)
                _loadedBehavior = (BehaviorNode)node;

            string content = xml.Value;
            if ((content == null) && (xml is XmlElement)&&(xml.ChildNodes .Count>0))
            {
                foreach (XmlNode child in xml.ChildNodes)
                {
                    if (child is XmlText)
                    {
                        content += child.InnerText;
                    }
                }
            }

            // initialise the properties
            IList<DesignerPropertyInfo> properties = node.GetDesignerProperties();
            foreach (DesignerPropertyInfo property in properties)
            {
                if (property.Attribute.HasFlags(DesignerProperty.DesignerFlags.NoSave))
                    continue;

                InitProperty(xml, node, property);
                if ((property.Property.Name == "content")&&(content != null))
                {
                    property.SetValueFromString(node, content);

                }
                node.PostPropertyInit(property);
            }

            // maintain compatibility with version 1
            if (node is ReferencedBehaviorNode)
            {
                ReferencedBehaviorNode refbehavior = (ReferencedBehaviorNode)node;
                if (refbehavior.ReferenceFilename == null)
                    refbehavior.ReferenceFilename = GetAttribute(xml, "Reference");
            }

            // update node with properties
            node.OnPropertyValueChanged(false);
            importEditNode(xml);
            // load child objects
            foreach (XmlNode xnode in xml.ChildNodes)
            {
                if (xnode.NodeType == XmlNodeType.Element)
                {
                    switch (xnode.Name.ToLowerInvariant())
                    {
                        // maintain compatibility with version 1
                        case ("node"):
                            node.AddChild(node.DefaultConnector, CreateNode(xnode));
                            break;

                        case ("event"):
                            node.AddSubItem(new Node.SubItemEvent(CreateEvent(node, xnode)));
                            break;

                        case ("comment"):
                            // create a comment object
                            node.CommentText = "temp";

                            // initialise the attributes
                            properties = node.CommentObject.GetDesignerProperties();
                            foreach (DesignerPropertyInfo property in properties)
                            {
                                if (property.Attribute.HasFlags(DesignerProperty.DesignerFlags.NoSave))
                                    continue;

                                string value;
                                if (GetAttribute(xnode, property.Property.Name, out value))
                                    property.SetValueFromString(node.CommentObject, value);
                            }
                            break;

                        case ("connector"):
                            string identifier = GetAttribute(xnode, "Identifier");
                            Brainiac.Design.Nodes.Node.Connector connector = node.GetConnector(identifier);

                            foreach (XmlNode connected in xnode.ChildNodes)
                            {
                                if (connected.NodeType == XmlNodeType.Element &&
                                    connected.Name.ToLowerInvariant() == "node")
                                {
                                    node.AddChildNotModified(connector, CreateNode(connected));
                                }
                            }
                            break;
                        default:
                            node.AddChild(node.DefaultConnector, CreateNode(xnode));
                            break;

                    }
                }
            }

            // update events with attributes
            if (node.SubItems.Count > 0)
            {
                foreach (Brainiac.Design.Nodes.Node.SubItem sub in node.SubItems)
                {
                    node.SelectedSubItem = sub;

                    node.OnSubItemPropertyValueChanged(false);
                }

                node.SelectedSubItem = null;
            }

            return node;
        }
        /// <summary>
        /// Loads a behaviour from the given filename
        /// </summary>
        public override void Load()
        {
            try
            {
                if (_filename.StartsWith("http://"))
                {
                    string behaviorText = HttpGet(_filename);
                    _xmlfile.LoadXml(behaviorText);
                }
                else
                {
                    _xmlfile.Load(_filename);
                }
                XmlNode root=null;
                if (_xmlfile.ChildNodes.Count>1)
                    root = _xmlfile.ChildNodes[1].ChildNodes[0];
                else
                    root = _xmlfile.ChildNodes[0];

                if (_filename.ToLower ().EndsWith(".btx"))
                {
                    Node    node = new Behavior();
                    node.AddChild(node.DefaultConnector, CreateNode(root));
                    _node = (BehaviorNode)node;

                }
                else
                {
                _node = (BehaviorNode)CreateNode(root);
                }   
                _node.FileManager = this;

                DoPostLoad((Node)_node);
            }
            catch
            {
                _xmlfile.RemoveAll();

                throw;
            }
        }
        public override void LoadWebDir(string behaviorServer, IList<string> FileList)
        {
            string listRequest = behaviorServer + @"/list/";
            string returnList = HttpGet(listRequest);
            string[] flist = returnList.Split('\n');
            int fcount = 0;
            foreach (string f in flist)
            {
                string f2 = f.Replace("\r", "");
                FileList.Add(f2);
                fcount++;
                //if (fcount > 1000) break;
            }
        }
        public string HttpGet(string url)
        {
            HttpWebRequest req = WebRequest.Create(url)
                                 as HttpWebRequest;
            string result = null;
            req.Timeout = 180000;
            using (HttpWebResponse resp = req.GetResponse()
                                          as HttpWebResponse)
            {
                StreamReader reader =
                    new StreamReader(resp.GetResponseStream());
                result = reader.ReadToEnd();
            }
            return result;
        }

 public  string HttpPost(string url,
     string[] paramName, string[] paramVal)
        {
            HttpWebRequest req = WebRequest.Create(new Uri(url))
                                 as HttpWebRequest;
            req.Method = "POST";
            req.ContentType = "application/x-www-form-urlencoded";

            // Build a string with all the params, properly encoded.
            // We assume that the arrays paramName and paramVal are
            // of equal length:
            StringBuilder paramz = new StringBuilder();
            for (int i = 0; i < paramName.Length; i++)
            {
                paramz.Append(paramName[i]);
                paramz.Append("=");
                paramz.Append(System.Uri.EscapeUriString(paramVal[i]));
                paramz.Append("&");
            }

            // Encode the parameters as form data:
            byte[] formData =
                UTF8Encoding.UTF8.GetBytes(paramz.ToString());
            req.ContentLength = formData.Length;

            // Send the request:
            using (Stream post = req.GetRequestStream())
            {
                post.Write(formData, 0, formData.Length);
            }

            // Pick up the response:
            string result = null;
            using (HttpWebResponse resp = req.GetResponse()
                                          as HttpWebResponse)
            {
                StreamReader reader =
                    new StreamReader(resp.GetResponseStream());
                result = reader.ReadToEnd();
            }

            return result;
        }
    }
}
