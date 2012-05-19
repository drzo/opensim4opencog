using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Net;
using System.Web;
using Brainiac.Design.Nodes;
using Brainiac.Design.Attributes;

namespace DaxPlugin
{
    public class DaxExporter : Brainiac.Design.Exporters.Exporter
    {
        		// the namespace the behaviours are exported to
		protected const string _usedNamespace= "Brainiac.Behaviors";

        public DaxExporter(BehaviorNode node, string outputFolder, string filename)
            : base(node, outputFolder, filename + ".btx")
		{

            if (filename.Length > 0)
            {
                if (filename.StartsWith("http://"))
                {
                    _filename = filename;
                }
                else
                {
                    _filename = Path.GetFullPath(filename);
                }
            }

		}
		protected void ExportBehavior(StreamWriter file, BehaviorNode behavior)
		{
            string filename = behavior.FileManager.Filename;
            string nameSpace = _usedNamespace ;
			string classname= Path.GetFileNameWithoutExtension(behavior.FileManager.Filename).Replace(" ", string.Empty);

            if (filename.StartsWith("http://"))
            {
                _filename = Path.GetFileName(filename);
                ExportBehaviorToServer(file, behavior);
            }
            else
            {
                _filename = Path.GetFullPath(filename);
                nameSpace = GetNamespace(_usedNamespace, _filename);
                classname = Path.GetFileNameWithoutExtension(behavior.FileManager.Filename).Replace(" ", string.Empty);

                file.Write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n");
                file.Write("<aiml version=\"1.0\">\r\n");
                file.Write("  <state name=\"*\">\r\n");


                // write comments
                file.Write(string.Format("\t<!-- Exported behavior: {0} --> \r\n", _filename));
                file.Write(string.Format("\t<!-- Exported file:     {0} -->\r\n\r\n", behavior.FileManager.Filename));

                // export nodes
                int nodeID = 0;
                // export the children
                foreach (Node child in ((Node)behavior).Children)
                    ExportNode(file, nameSpace, behavior, "this", child, 3, ref nodeID);

                // close constructor
                file.Write("  </state>\r\n");
                file.Write("</aiml>\r\n");
            }
        }

        protected void ExportBehaviorToServer(StreamWriter file, BehaviorNode behavior)
        {
            if (_behaviorServer == null) return;
            if (_behaviorServer.Length == 0) return;

            string namspace = GetNamespace(_usedNamespace, _filename);
            string classname = Path.GetFileNameWithoutExtension(behavior.FileManager.Filename).Replace(" ", string.Empty);
            string behaviorURL = _behaviorServer + "/behavior/"+classname+".BTX";

            HttpWebRequest httpRequest = (HttpWebRequest)WebRequest.Create(behaviorURL);
            httpRequest.Method = WebRequestMethods.Http.Post;

            //httpRequest.ContentType = "application/x-www-form-urlencoded";
            httpRequest.ContentType = "text/xml";
            //httpRequest.ContentLength = 0;
            StreamWriter requestWriter = new StreamWriter(httpRequest.GetRequestStream(), System.Text.Encoding.ASCII);
            
           // requestWriter.Write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n");
           // requestWriter.Write("<aiml version=\"1.0\">\r\n");
           // requestWriter.Write("  <state name=\"*\">\r\n");


            // write comments
           // requestWriter.Write(string.Format("\t<!-- Exported behavior: {0} --> \r\n", _filename));
           // requestWriter.Write(string.Format("\t<!-- Exported file:     {0} -->\r\n\r\n", behavior.FileManager.Filename));

            // export nodes
            int nodeID = 0;
            // export the children
            foreach (Node child in ((Node)behavior).Children)
                ExportNode(requestWriter, namspace, behavior, "this", child, 3, ref nodeID);

            // close constructor
           // requestWriter.Write("  </state>\r\n");
            //requestWriter.Write("</aiml>\r\n");

            requestWriter.Close();
            WebResponse responseGet = httpRequest.GetResponse();

        }
       
        protected virtual void ExportConstructorAndProperties(StreamWriter file, Node node, string indent, string nodeName, string classname)
        {
            // create a new instance of the node
            //file.Write(string.Format("{0}\t{2} {1} = new {2}();\r\n", indent, nodeName, classname));
            string tag = classToTag(classname);
            file.Write(string.Format("{0}<{2}", indent, nodeName,tag));

            // assign all the properties
            ExportProperties(file, nodeName, node, indent);
            file.Write(string.Format(">", indent, nodeName, classname));
            if (!hasContent (tag)) file.Write(string.Format("\r\n"));

            ExportContents(file, nodeName, node, indent);
        }
        /// <summary>
        /// Exports a node to the given file.
        /// </summary>
        /// <param name="file">The file we want to export to.</param>
        /// <param name="namspace">The namespace of the behaviour we are currently exporting.</param>
        /// <param name="behavior">The behaviour we are currently exporting.</param>
        /// <param name="parentName">The name of the variable of the node which is the parent of this node.</param>
        /// <param name="node">The node we want to export.</param>
        /// <param name="indentDepth">The indent of the ocde we are exporting.</param>
        /// <param name="nodeID">The current id used for generating the variables for the nodes.</param>
        protected void ExportNode(StreamWriter file, string namspace, BehaviorNode behavior, string parentName, Node node, int indentDepth, ref int nodeID)
        {
            // generate some data
            string classname = node.ExportClass;
            string nodeName = string.Format("node{0}", ++nodeID);

            // generate the indent string
            string indent = string.Empty;
            for (int i = 0; i < indentDepth; ++i)
                indent += '\t';

            // we have to handle a referenced behaviour differently
            if (node is ReferencedBehaviorNode)
            {
                // generate the namespace and name of the behaviour we are referencing
                string refRelativeFilename = behavior.MakeRelative(((ReferencedBehaviorNode)node).ReferenceFilename);
                string refNamespace = GetNamespace(namspace, refRelativeFilename);
                string refBehaviorName = Path.GetFileNameWithoutExtension(((ReferencedBehaviorNode)node).ReferenceFilename.Replace(" ", string.Empty));

                // simply add the instance of the behaviours we are referencing
                file.Write(string.Format("{0}{1}.AddChild({1}.GetConnector(\"{2}\"), {3}.{4}.Instance);\r\n", indent, parentName, node.ParentConnector.Identifier, refNamespace, refBehaviorName));
            }
            else
            {
                // open some brackets for a better formatting in the generated code
                //file.Write(string.Format("{0}{{\r\n", indent));

                // export the constructor and the properties
                ExportConstructorAndProperties(file, node, indent, nodeName, classname);

                // add the node to its parent
                //file.Write(string.Format("{0}\t{1}.AddChild({1}.GetConnector(\"{2}\"), {3});\r\n", indent, parentName, node.ParentConnector.Identifier, nodeName));

                // export the child nodes
                foreach (Node child in node.Children)
                    ExportNode(file, namspace, behavior, nodeName, child, indentDepth + 1, ref nodeID);

                // close the brackets for a better formatting in the generated code
                //file.Write(string.Format("{0}}}\r\n", indent));
                string tag = classToTag(classname);
                if (hasContent(tag))
                    file.Write(string.Format("</{1}>\r\n", indent, tag));
                else
                    file.Write(string.Format("{0}</{1}>\r\n", indent, tag));
            }
        }
        public bool hasContent(string tag)
        {
            return (
                    (tag == "li") 
                    || (tag == "pattern") 
                    || (tag == "that")
                    || (tag == "subbehavior")
                    || (tag == "chat")
                    || (tag == "flushqueue")
                    || (tag == "enqueue")
                    || (tag == "assert")
                    || (tag == "chat")
                     );
        }
        public string classToTag(string classname)
        {
            classname = classname.Replace("DaxPlugin.Nodes.Dax", "");
            classname = classname.Replace("DaxPlugin.Nodes.Dax", "");
            classname = classname.ToLower();
            return classname;

        }
        /// <summary>
        /// Exports all the properties of a node and assigns them.
        /// </summary>
        /// <param name="file">The file we are exporting to.</param>
        /// <param name="nodeName">The name of the node we are setting the properties for.</param>
        /// <param name="node">The node whose properties we are exporting.</param>
        /// <param name="indent">The indent for the currently generated code.</param>
        protected void ExportProperties(StreamWriter file, string nodeName, Node node, string indent)
        {
            //file.Write(string.Format("[id ='{1}'] ", indent, nodeName));
            // export all the properties
            IList<DesignerPropertyInfo> properties = node.GetDesignerProperties();
            for (int p = 0; p < properties.Count; ++p)
            {
                // we skip properties which are not marked to be exported
                if (properties[p].Attribute.HasFlags(DesignerProperty.DesignerFlags.NoExport))
                    continue;
                if (properties[p].Property.Name.ToLower() == "content")
                    continue;

                // create the code which assigns the value to the node's property
               // file.Write(string.Format("{0}\t{1}.{2} = {3};\r\n", indent, nodeName, properties[p].Property.Name, properties[p].GetExportValue(node)));
                string expVal = properties[p].GetExportValue(node);
                if (!expVal.StartsWith("\"")) expVal = String.Format("\"{0}\"", expVal);
                file.Write(string.Format(" {2} = {3}", indent, nodeName, properties[p].Property.Name.ToLower(), expVal));
            }
        }
        protected void ExportContents(StreamWriter file, string nodeName, Node node, string indent)
        {
            //file.Write(string.Format("[id ='{1}'] ", indent, nodeName));
            // export all the properties
            IList<DesignerPropertyInfo> properties = node.GetDesignerProperties();
            for (int p = 0; p < properties.Count; ++p)
            {
                // we skip properties which are not marked to be exported
                if (properties[p].Attribute.HasFlags(DesignerProperty.DesignerFlags.NoExport))
                    continue;
                if (properties[p].Property.Name.ToLower() != "content")
                    continue;
                // create the code which assigns the value to the node's property
                // file.Write(string.Format("{0}\t{1}.{2} = {3};\r\n", indent, nodeName, properties[p].Property.Name, properties[p].GetExportValue(node)));
                file.Write(string.Format("{3}", indent, nodeName, properties[p].Property.Name.ToLower(), dequote(properties[p].GetExportValue(node))));
            }
        }
        public string dequote(string msg)
        {
            if ((msg.Length > 2) && (msg.StartsWith("\"")) && msg.EndsWith("\""))
            {
                msg = msg.Substring(1, msg.Length - 2);
            }
            return msg;
        }
        /// <summary>
        /// Generates the namespace used for a referenced behaviour.
        /// </summary>
        /// <param name="currentNamespace">The namespace we are currently at.</param>
        /// <param name="relativeFilename">The relative filename of the ebhaviour we are referencing.</param>
        /// <returns></returns>
        protected string GetNamespace(string currentNamespace, string relativeFilename)
        {
            // if we stay in the same folder/namespace, just return the current one.
            if (Path.GetFileName(relativeFilename) == relativeFilename)
                return currentNamespace;

            // generate the namespace we are using
            string file = Path.GetDirectoryName(currentNamespace.Replace('.', '\\') + '\\' + relativeFilename);

            // make sure we remove any ..\ we found
            string full = Path.GetFullPath(file);
            string folder = Path.GetFullPath(".");

            //Debug.Check(full.StartsWith(folder));

            // get a relative path for the namespace
            string namespaceFile = full.Substring(folder.Length + 1);

            // turn the path into a namespace and remove all spaces
            return namespaceFile.Replace('\\', '.').Replace(" ", string.Empty);
        }
        /// <summary>
        /// Export the assigned node to the assigned file.
        /// </summary>
        public override void Export()
        {
            // get the abolute folder of the file we want toexport
            string outfile = _filename;
            string folder = Path.GetDirectoryName(outfile);

            if (folder.Length == 0)
            {
                outfile = _outputFolder + '\\' + _filename;
                folder = Path.GetDirectoryName(outfile);
            }

            // if the directory does not exist, create it
            if (!Directory.Exists(folder))
                Directory.CreateDirectory(folder);

            // export to the file
            StreamWriter file = new StreamWriter(outfile);
            ExportBehavior(file, _node);
            file.Close();
        }


        public string HttpGet(string url)
        {
            HttpWebRequest req = WebRequest.Create(url)
                                 as HttpWebRequest;
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

        public string HttpPost(string url,
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
        public string HttpPost(string url,
            string bodyString)
        {
            HttpWebRequest req = WebRequest.Create(new Uri(url))
                                 as HttpWebRequest;
            req.Method = "POST";
            req.ContentType = "application/x-www-form-urlencoded";

            // Build a string with all the params, properly encoded.
            // We assume that the arrays paramName and paramVal are
            // of equal length:
            StringBuilder paramz = new StringBuilder();
            paramz.Append(System.Uri.EscapeUriString(bodyString));

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
