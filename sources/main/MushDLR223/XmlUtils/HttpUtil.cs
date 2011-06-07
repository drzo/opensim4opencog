using System;
using System.IO;
using System.Net;
using System.Text;
using System.Xml;
using System.Xml.XPath;
using Sgml;

namespace MushDLR223.Utilities
{
    public static class HttpUtil
    {
        public static string GetUrlData(string url)
        {
            WebClient web = new WebClient();
            WebHeaderCollection headers = new WebHeaderCollection();
            headers[HttpRequestHeader.ContentType] = "application/x-www-form-urlencoded; charset=utf-8";
            headers[HttpRequestHeader.Referer] = "http://translate.google.cn/";
            web.Headers = headers;
            byte[] bystr = web.DownloadData(url);
            return  Encoding.UTF8.GetString(bystr);
        }

        public static string GetWellFormedHTML(string html, string xpathNavPath)
        {
            // StreamReader sReader = null;
            StringWriter sw = null;
            SgmlReader reader = null;
            XmlTextWriter writer = null;
            try
            {
                //  if (uri == String.Empty) uri = "http://www.XMLforASP.NET";
                // HttpWebRequest req = (HttpWebRequest)WebRequest.Create(uri);
                //  HttpWebResponse res = (HttpWebResponse)req.GetResponse();
                //  sReader = new StreamReader(res.GetResponseStream());
                reader = new SgmlReader();
                reader.DocType = "HTML";
                reader.InputStream = new StringReader(html);
                sw = new StringWriter();
                writer = new XmlTextWriter(sw);
                writer.Formatting = Formatting.Indented;
                //writer.WriteStartElement("Test");
                while (reader.Read())
                {
                    if (reader.NodeType != XmlNodeType.Whitespace)
                    {
                        writer.WriteNode(reader, true);
                    }
                }
                //writer.WriteEndElement();
                if (xpathNavPath == null)
                {
                    string sr = sw.ToString();
                    sr = sr.Replace("\r", "\n");
                    sr = sr.Replace("\n\n", "\n");
                    return sr;
                }
                else
                { //Filter out nodes from HTML
                    StringBuilder sb = new StringBuilder();
                    XPathDocument doc = new XPathDocument(new StringReader(sw.ToString()));
                    XPathNavigator nav = doc.CreateNavigator();
                    XPathNodeIterator nodes = nav.Select(xpathNavPath);
                    while (nodes.MoveNext())
                    {
                        sb.Append(nodes.Current.Value + "\n");
                    }
                    string sr = sb.ToString();
                    sr = sr.Replace("\r", "\n");
                    sr = sr.Replace("\n\n", "\n");
                    return sr;
                }
            }
            catch (Exception exp)
            {
                writer.Close();
                reader.Close();
                sw.Close();
                // sReader.Close();
                return exp.Message;
            }
        }
    }
}