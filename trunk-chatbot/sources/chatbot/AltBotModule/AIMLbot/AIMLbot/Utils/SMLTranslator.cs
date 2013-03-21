#region

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using LogicalParticleFilter1;
using MushDLR223.ScriptEngines;

#endregion

namespace AltAIMLbot.Utils
{
    public class SMLTranslator
    {
        //Based on ideas from :http://www.o-xml.org/projects/sml.html
        // an independent re-implementation
        // the xml->sml does not preserve whitespace, while sml->xml with literal should
        // option strings determine which tags require leading or trailing line breaks
        //   for formatting
        //
        // example: 
        // aiml: <category><pattern>I AM HERE</pattern><template>We are together now.</template></category>
        //  sml: category{ pattern{"I AM HERE"} template{"We are togther now."}}
        //
        // aiml: <star/>
        //  sml: star;

        static void spaceN(int d)
        {
            if (d == 0) return;
            for (int i = 1; i < d; i++)
            {
                Console.Write(" ");
            }
        }
        static void spaceN(StringWriter sw, int d)
        {
            if (d == 0) return;
            for (int i = 1; i < d; i++)
            {
                sw.Write(" ");
            }
        }

        // sml 2 xml options
        public static string onelinetags = "pattern,li,srai,setaimlvar,chat,refserver,push";
        public static string breaktags = "category,state,behavior";
        public static bool literal = true;
        // xml 2 sml options
        public static string newlinetags = "aiml";
        public static string prelinetags = "category,state,behavior,selector,sequence,parallel,task";




        static int CharIndexOf(char[] buf, char x, int startpoint)
        {
            int max = buf.Length;
            for (int i = startpoint; i < max; i++)
            {

                    if (buf[i] == x) return i;
            }
            return -1;
        }
        static int CharIndexOfAny(char[] buf, char[] x, int startpoint)
        {
            int max = buf.Length;
            int tmax = x.Length ;
            for (int i = startpoint; i < max; i++)
            {
                foreach (char c in x)
                {
                    if (buf[i]==c) return i;
                }
            }
            return -1;
        }
        static int CharIndexOfEscQuote(char[] buf, int startpoint,int absmax)
        {
            int max = buf.Length;
            for (int i = startpoint; (i < max)&&(i<absmax); i++)
            {
                if (buf[i] == '\\' && buf[i + 1] == '"' )
                    return i;
            }
            return -1;
        }

        static int endWhiteSpace(char[] buf, int startpoint)
        {
            int max = buf.Length;
            for (int i = startpoint; i < max; i++)
            {
                char fc = buf[i];
                if (fc != '\r' && fc != '\n' && fc != '\t' && fc != ' ') return i;
            }
            return max;
        }


        public static int sml2xml(StringWriter sw, char[] smltext, int smlp, int d)
        {
            int remainder = smlp;
            if (smlp >= smltext.Length) return smlp;
            char[] wspc = { '\r', '\n','\t',' ' };

            // manual front whitespace trimmer
            char firstchar = smltext[smlp];
            while (firstchar == '\r' || firstchar == '\n' || firstchar == '\t' || firstchar == ' ')
            {
                 sw.Write("{0}", firstchar);
                 smlp++;
                firstchar = smltext[smlp];
            }

            string frontseg = new string(smltext,smlp, 4);
            // Handle the header
            if (frontseg.StartsWith("!sml"))
            {
                int p1 = CharIndexOf(smltext,'!', smlp);
                int p2 = CharIndexOf(smltext,'!', p1 + 1);
                string etext = new string(smltext,smlp, p2 - smlp);// smltext.Substring(0, p2);
                //string[] texts = smltext.Split('!');
                string[] texts = etext.Split('!');
                string deText = texts[1];
                deText = deText.Replace("sml", "xml");
                sw.Write("<?{0}?>", deText);
                int rest1 = smlp+deText.Length + 2;
                remainder = sml2xml(sw, smltext,rest1, d + 1);
                return remainder;
            }

            //#text
            if (frontseg.StartsWith("\""))
            {
                //smltext = smltext.Replace("\\\"", @"<&quot;/>");
                int p1 = CharIndexOf(smltext, '"', smlp);
                int p2 = CharIndexOf(smltext, '"', p1 + 1);
                int q1 = CharIndexOfEscQuote(smltext,  smlp,p2);
                int bufmax = smltext.Length;
                if ((q1 >= 0) && (q1 > p1) && (q1 < p2))
                {
                    int q2 = CharIndexOfEscQuote(smltext, q1 + 1, bufmax);
                    p2 = CharIndexOf(smltext, '"', q2 + 2);
                    while (smltext[p2 - 1]=='\\' && smltext[p2]=='"')
                    {
                        q2 = CharIndexOfEscQuote(smltext, p2 + 1, bufmax);
                        p2 = CharIndexOf(smltext, '"', q2 + 2);
                    }
                }
                else
                {
                    p2 = p2;
                }

                string qtext = new string (smltext,smlp, (p2 + 1)-smlp);
                qtext = qtext.Replace("\\\"", @"<&quot;/>");
                //string[] texts2 = smltext.Split('"');
                string[] texts = qtext.Split('"');
                string deText = texts[1];
                //remainder = smltext.Substring(deText.Length + 2);
                remainder = p2 + 1;
                deText = deText.Replace(@"<&quot;/>", @"""");
                //remainder = remainder.Replace(@"<&quot;/>", "\\\"");
                sw.Write("{0}", deText);
                return remainder;
            }

            if (frontseg.StartsWith("//"))
            {
                int p1 = CharIndexOf(smltext, '\n',smlp);
                int p2 = CharIndexOf(smltext, '\n', p1 + 1);
                string ntext = new string (smltext,smlp, p2-smlp);

                //string[] texts2 = smltext.Split('\n');
                string[] texts = ntext.Split('\n');
                string deText = texts[0];
                deText = deText.Substring(2);
                sw.WriteLine("<!-- {0} -->", deText);
                remainder = smlp+texts[0].Length + 1;
                return remainder;
            }
            char[] seps = { '{', ';' };
            int firstSep = CharIndexOfAny(smltext,seps,smlp);
            string[] front = new string[2]; //smltext.Split(seps);
            if (firstSep > 0)
            {
                front[0] = new string (smltext,smlp, firstSep-smlp);
            }
            //if (front.Length == 0) return smltext;
            if (firstSep < 0)
                return smlp;
            // end of tag
            if (smltext[smlp]=='}')
            {
                return smlp+1;
            }
            // end of tag
            if (smltext[smlp] == ';')
            {
                //sw.Write("/>");
                sw.Write("<{0}/>", front[0]);
                //sw.Write("<{0}/>", smltext.Substring(0, firstSep - 1));
                 return smlp+1;
            }
            // Empty tag
            // should be any white space
            if ((smltext[smlp] == '{')&&(smltext[smlp+1] == '}'))
            {
                //sw.Write("/>");
                sw.Write("<{0}/>", front[0]);
                //sw.Write("<{0}/>", smltext.Substring(0, firstSep - 1));
                return smlp + 2;
            }

            //general tag
            string[] tagd = Regex.Split(front[0], "[^A-Za-z0-9]+");
            string tag = tagd[0];
            if (tag.Trim() == "" && (tagd.Length > 1)) tag = tagd[1];
            int restp = smlp+tag.Length;
            // Short quick close tag for "elem a1='v1' a2='v2';"
            int restp2 = smlp+front[0].Length;
            if (smltext[restp2]==';')
            {
                sw.Write("<{0}/>", front[0]);
                return restp2+1;
            }
            //Regular tag (with more than zero children)
            // Write parent tag
            if (breaktags.Contains(tag.ToLower()) && !literal) sw.WriteLine();
            if (literal)
            {
                sw.Write("<{0}>", front[0]);
            }
            else
            {
                if (onelinetags.Contains(tag.ToLower()))
                {
                    spaceN(sw, d); sw.Write("<{0}>", front[0].Trim());
                }
                else
                {
                    spaceN(sw, d); sw.WriteLine("<{0}>", front[0].Trim());
                }
            }
            restp = smlp+front[0].Length;

            //Process Children
            //rest = rest.TrimStart();
            restp=endWhiteSpace(smltext, restp);

            if (smltext[restp] == ';')
            {
                sw.Write("<{0}/>", tag);
                return restp+1;
            }

            if (smltext[restp] == '{')
            {
                int bufmax = smltext.Length;
                int childRemainder = sml2xml(sw, smltext, restp+1, d + 1);

                int p1 = CharIndexOf(smltext,'}',childRemainder);
                if (p1 < 0) 
                    return childRemainder;

                string head = new string (smltext,childRemainder,( p1 + 1) - childRemainder );

                //while (!childRemainder.TrimStart().StartsWith("}") && childRemainder.Length > 0)
                while (!head.TrimStart().StartsWith("}") && ((bufmax-childRemainder) >0) )
                {
                    //string ctext = childRemainder;
                    int cLen = bufmax-childRemainder;
                    restp = endWhiteSpace( smltext,childRemainder);
                    int cwhitespaceLen = cLen - (bufmax-restp);
                    if ((literal) && (cwhitespaceLen > 0))
                    {
                        //string slice = ctext.Substring(0, cwhitespaceLen);
                        string slice = new string (smltext,childRemainder, cwhitespaceLen);
                        sw.Write("{0}", slice);
                    }

                    childRemainder = sml2xml(sw, smltext,restp, d + 1);

                    p1 = CharIndexOf(smltext, '}', childRemainder);
                    if (p1 < 0)
                        break;
                        //return childRemainder;

                    head = new string(smltext,childRemainder, (p1 + 1) - childRemainder);

                }
                if ((bufmax - childRemainder) > 0)
                {
                    //string ctext = childRemainder;
                    int cLen = bufmax - childRemainder;
                    restp = endWhiteSpace(smltext, childRemainder);
                    int cwhitespaceLen = cLen - (bufmax - restp);
                    if ((literal) && (cwhitespaceLen > 0))
                    {
                        //string slice = ctext.Substring(0, cwhitespaceLen);
                        string slice = head.Substring(0, cwhitespaceLen);
                        sw.Write("{0}", slice);
                    }

                    remainder = restp+1;
                }
                else
                    remainder = childRemainder;
            }
            // Close Parent tag
            if (literal)
            {
                sw.Write("</{0}>", tag);
            }
            else
            {
                if (!onelinetags.Contains(tag.ToLower()))
                {
                    if (!sw.ToString().EndsWith("\n")) sw.WriteLine();
                    spaceN(sw, d);
                }
                sw.WriteLine("</{0}>", tag);
                if (breaktags.Contains(tag.ToLower())) sw.WriteLine();
            }
            return remainder;
        }


        public static string sml2xml(StringWriter sw, string smltext, int d)
        {
            if (smltext.Length == 0) return "";
            string remainder = "";
            /*
            string otext = smltext;
            int origLen = otext.Length;
            smltext = smltext.TrimStart();
            int whitespaceLen = origLen - smltext.Length;
            if ((literal) && (whitespaceLen > 0))
            {
                string fslice = otext.Substring(0, whitespaceLen);
                sw.Write("{0}", fslice);
            }
            */

            // manual front whitespace trimmer
             char[] wspc = { '\r', '\n','\t',' ' };
             string car = smltext.Substring(0, 1);
             int pc = car.IndexOfAny(wspc);
             while (pc == 0)
             {
                 smltext = smltext.Substring(1);
                 sw.Write("{0}", car);
                 car = smltext.Substring(0, 1);
                 pc = car.IndexOfAny(wspc);
             }

            //spaceN(sw, whitespaceLen);
            // Handle the header
            if (smltext.StartsWith("!sml"))
            {
                int p1 = smltext.IndexOf('!');
                int p2 = smltext.IndexOf('!', p1 + 1);
                string etext = smltext.Substring(0, p2);
                //string[] texts = smltext.Split('!');
                string[] texts = etext.Split('!');
                string deText = texts[1];
                deText = deText.Replace("sml", "xml");
                sw.Write("<?{0}?>", deText);
                string rest1 = smltext.Substring(deText.Length + 2);
                remainder = sml2xml(sw, rest1, d + 1);
                return remainder;

            }
            //#text
            if (smltext.StartsWith("\""))
            {
                //smltext = smltext.Replace("\\\"", @"<&quot;/>");
                int p1 = smltext.IndexOf('"');
                int p2 = smltext.IndexOf('"', p1 + 1);
                int q1 = smltext.IndexOf("\\\"");
                if ((q1 >= 0) && (q1 > p1) && (q1 < p2))
                {
                    int q2 = smltext.IndexOf("\\\"", q1 + 1);
                    p2 = smltext.IndexOf('"', q2 + 2);
                    while (smltext.Substring(p2 - 1, 2).StartsWith("\\\""))
                    {
                        q2 = smltext.IndexOf("\\\"", p2 + 1);
                        p2 = smltext.IndexOf('"', q2 + 2);
                    }
                }
                else
                {
                    p2 = p2;
                }
                
                string qtext = smltext.Substring(0, p2+1);
                qtext = qtext.Replace("\\\"", @"<&quot;/>");
                //string[] texts2 = smltext.Split('"');
                string[] texts = qtext.Split('"');
                string deText = texts[1];
                //remainder = smltext.Substring(deText.Length + 2);
                remainder = smltext.Substring(p2 + 1);
                deText = deText.Replace(@"<&quot;/>", @"""");
                //remainder = remainder.Replace(@"<&quot;/>", "\\\"");
                sw.Write("{0}", deText);
                return remainder;
            }

            if (smltext.StartsWith("//"))
            {
                int p1 = smltext.IndexOf('\n');
                int p2 = smltext.IndexOf('\n', p1 + 1);
                string ntext = smltext.Substring(0, p2);

                //string[] texts2 = smltext.Split('\n');
                string[] texts = ntext.Split('\n');
                string deText = texts[0];
                deText = deText.Substring(2);
                sw.WriteLine("<!-- {0} -->", deText);
                remainder = smltext.Substring(texts[0].Length + 1);
                return remainder;
            }
            char[] seps = { '{', ';' };
            int firstSep = smltext.IndexOfAny(seps);
            string[] front = new string[2]; //smltext.Split(seps);
            if (firstSep > 0)
            {
                front[0] = smltext.Substring(0, firstSep);
            }
            //if (front.Length == 0) return smltext;
            if (firstSep < 0) 
                return smltext;
            // end of tag
            if (smltext.StartsWith("}"))
            {
                return smltext.Substring(1);
            }
            // end of tag
            if (smltext.StartsWith(";"))
            {
                //sw.Write("/>");
                sw.Write("<{0}/>",front[0]);
                //sw.Write("<{0}/>", smltext.Substring(0, firstSep - 1));
                return smltext.Substring(1);
            }
            // Empty tag
            // should be any white space
            if (smltext.StartsWith("{}"))
            {
                //sw.Write("/>");
                sw.Write("<{0}/>", front[0]);
                //sw.Write("<{0}/>", smltext.Substring(0, firstSep - 1));
                return smltext.Substring(2);
            }

            //general tag
            string[] tagd = Regex.Split(front[0], "[^A-Za-z0-9]+");
            string tag = tagd[0];
            if (tag.Trim() == "" && (tagd.Length > 1)) tag = tagd[1];
            string rest = smltext.Substring(tag.Length);
            // Short quick close tag for "elem a1='v1' a2='v2';"
            string rest2 = smltext.Substring(front[0].Length);
            if (rest2.StartsWith(";"))
            {
                sw.Write("<{0}/>", front[0]);
                return rest2.Substring(1);
            }
            //Regular tag (with more than zero children)
            // Write parent tag
            if (breaktags.Contains(tag.ToLower()) && !literal) sw.WriteLine();
            if (literal)
            {
                sw.Write("<{0}>", front[0]);
            }
            else
            {
                if (onelinetags.Contains(tag.ToLower()))
                {
                    spaceN(sw, d); sw.Write("<{0}>", front[0].Trim());
                }
                else
                {
                    spaceN(sw, d); sw.WriteLine("<{0}>", front[0].Trim());
                }
            }
            rest = smltext.Substring(front[0].Length);
            //Process Children
            rest = rest.TrimStart();
            if (rest.StartsWith(";"))
            {
                sw.Write("<{0}/>", tag);
                return rest.Substring(1);
            }

            if (rest.StartsWith("{"))
            {
                string childRemainder = sml2xml(sw, rest.Substring(1), d + 1);

                int p1 = childRemainder.IndexOf('}');
                string head = childRemainder.Substring(0, p1+1);

                //while (!childRemainder.TrimStart().StartsWith("}") && childRemainder.Length > 0)
                while (!head.TrimStart().StartsWith("}") && childRemainder.Length > 0)
                {
                    //string ctext = childRemainder;
                    int cLen = childRemainder.Length;
                    rest = childRemainder.TrimStart();
                    int cwhitespaceLen = cLen - rest.Length;
                    if ((literal) && (cwhitespaceLen >0))
                    {
                        //string slice = ctext.Substring(0, cwhitespaceLen);
                        string slice = childRemainder.Substring(0, cwhitespaceLen);
                        sw.Write("{0}", slice);
                    }

                    childRemainder = sml2xml(sw, rest, d + 1);
                    p1 = childRemainder.IndexOf('}');
                    head = childRemainder.Substring(0, p1+1);

                }
                if (childRemainder.Length > 0)
                {
                    //string ctext = childRemainder;
                    int cLen = childRemainder.Length;
                    rest = childRemainder.TrimStart();
                    int cwhitespaceLen = cLen - rest.Length;
                    if ((literal) && (cwhitespaceLen > 0))
                    {
                        //string slice = ctext.Substring(0, cwhitespaceLen);
                        string slice = head.Substring(0, cwhitespaceLen);
                        sw.Write("{0}", slice);
                    }

                    remainder = rest.Substring(1);
                }
                else
                    remainder = childRemainder;
            }
            // Close Parent tag
            if (literal)
            {
                sw.Write("</{0}>", tag);
            }
            else
            {
                if (!onelinetags.Contains(tag.ToLower()))
                {
                    if (!sw.ToString().EndsWith("\n")) sw.WriteLine();
                    spaceN(sw, d);
                }
                sw.WriteLine("</{0}>", tag);
                if (breaktags.Contains(tag.ToLower())) sw.WriteLine();
            }
            return remainder;
        }

        public static string sml2xml0(StringWriter sw, string smltext, int d)
        {
            //Console.WriteLine("IN:'{0}' ({1})", smltext, d);
            string remainder = "";
            if (smltext.Length == 0) return "";
            if (smltext.StartsWith("}")) return smltext.Substring(1);
            if (smltext.StartsWith(";"))
            {
                //sw.Write("/>");
                return smltext.Substring(1);
            }
            if (smltext.StartsWith("\""))
            {
                string[] texts = smltext.Split('"');
                string deText = texts[1];
                sw.Write("{0}", deText);
                remainder = sml2xml(sw, smltext.Substring(deText.Length + 2).Trim(), d + 1);
                //remainder = smltext.Substring(deText.Length + 2).Trim();
                return remainder;
            }
            string[] front = smltext.Split('{');

            if (front.Length == 0) return smltext;
            // it is either "{tag attrib='value' ...{ ...}... }"
            // or "tag attrib='value' ...{...}"
            // or "{'text'}"
            foreach (string x in front)
            {
                //Console.WriteLine("{0}", x);
            }
            if (front[0].Length == 0)
            {
                sw.WriteLine(">");
                remainder = sml2xml(sw, smltext.Substring(1).Trim(), d + 1);
                return remainder;
            }
            string[] tagd = Regex.Split(front[0], "[^A-Za-z0-9]+");
            string tag = tagd[0].Trim();
            if (tag == "" && (tagd.Length > 1)) tag = tagd[1].Trim();
            string rest = smltext.Substring(tagd[0].Length).Trim();
            if (tagd.Length > 1)
            {
                if (rest.StartsWith(";"))
                {
                    sw.Write("<{0}/>", tag);
                    return rest.Substring(1);
                }
            }
            else
            {
            }

            if (breaktags.Contains(tag.ToLower())) sw.WriteLine();
            if (onelinetags.Contains(tag.ToLower()))
            {
                spaceN(sw, d); sw.Write("<{0}>#0", front[0].Trim());
            }
            else
            {
                spaceN(sw, d); sw.WriteLine("<{0}>#1", front[0].Trim());
            }
            remainder = sml2xml(sw, smltext.Substring(front[0].Length + 1).Trim(), d + 1);
            if (!onelinetags.Contains(tag.ToLower()))
            {
                if (!sw.ToString().EndsWith("\n")) sw.WriteLine();
                spaceN(sw, d);
            }
            sw.WriteLine("</{0}>", tag);
            if (breaktags.Contains(tag.ToLower())) sw.WriteLine();
            string remainder2 = sml2xml(sw, remainder.Trim(), d + 1);

            return remainder2;
        }

        public static void xmlInner(StringWriter sw, XmlNode currentNode, int d)
        {
            if (currentNode == null) return;
            string currentNodeName = currentNode.Name.ToLower();
            if (currentNodeName == "xml")
            {
                sw.Write("!sml {0}!\n", currentNode.Value);
                return;
            }

            if (currentNodeName == "#text")
            {
                sw.Write("\"{0}\"", currentNode.Value);
                return;
            }

            if (currentNodeName == "#comment")
            {
                sw.WriteLine("// {0}", currentNode.Value);
                return;
            }
            if (currentNodeName == "#whitespace")
            {
                sw.Write("{0}", currentNode.Value);
                return;
            }

            if (currentNodeName == "#document")
            {
                ////sw.Write("DOC");
                foreach (XmlNode child in currentNode.ChildNodes)
                {
                    xmlInner(sw, child, d + 1);
                }
                return;
            }
            else
            {
                if ((prelinetags.Contains(currentNodeName.ToLower())) &&
                    (!sw.ToString().StartsWith("\n")))
                {
                    sw.WriteLine();
                }
                spaceN(sw, d);
                sw.Write(currentNodeName);
            }

            if (currentNode.Attributes != null)
            {
                foreach (XmlAttribute attrib in currentNode.Attributes)
                {
                    sw.Write(" {0}=\"{1}\"", attrib.Name, attrib.Value);
                }
            }
            if ((currentNode.ChildNodes == null) || (currentNode.ChildNodes.Count == 0))
            {
                sw.Write(";");
            }
            else
            {
                sw.Write(" {");
                if (!onelinetags.Contains(currentNodeName.ToLower()) ||
                     newlinetags.Contains(currentNodeName.ToLower()))
                    sw.Write("\n");
                foreach (XmlNode child in currentNode.ChildNodes)
                {
                    xmlInner(sw, child, d + 1);
                }
                if (!sw.ToString().EndsWith("\""))
                    spaceN(sw, d);
                sw.Write("}\n");
            }

        }


        public static void xml2sml(StringWriter sw, string xmltext)
        {
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(xmltext);
            xmlInner(sw, doc, 0);
            //sw.WriteLine("------");
            //sw.WriteLine(xmltext);
        }

        public static string xml2sml(string xmltext)
        {
            StringWriter sw = new StringWriter();
            xml2sml(sw, xmltext);
            return sw.ToString();
        }

        public static string sml2xml00(string smltext)
        {
            StringWriter sw = new StringWriter();
            sml2xml(sw, smltext, 0);
            return sw.ToString();
        }
        public static string sml2xml(string smltext)
        {
            StringWriter sw = new StringWriter();
            char[] smlbuf = smltext.ToCharArray();
            sml2xml(sw, smlbuf,0, 0);
            return sw.ToString();
        }

        public static bool isSML(string smltext)
        {
            return smltext.Contains("!sml ");
        }

        public static Stream openSMLStream(string filename)
        {
            // Will load file then create an internal memory based stream
            // Check if it is SML and if so convert to XML
            string smlDoc = "";
            if (File.Exists(filename))
            {
                System.IO.StreamReader myFile = new System.IO.StreamReader(filename);
                smlDoc = myFile.ReadToEnd();
                myFile.Close();
                if (isSML(smlDoc))
                {
                    smlDoc = sml2xml(smlDoc);
                }
            }
            byte[] byteArray = Encoding.ASCII.GetBytes(smlDoc);
            var stream = new MemoryStream(byteArray);
            return stream;
        }
        public static String getSMLFile(string filename)
        {
            // Will load file then create an internal memory based stream
            // Check if it is SML and if so convert to XML
            string smlDoc = "";
            if (File.Exists(filename))
            {
                System.IO.StreamReader myFile = new System.IO.StreamReader(filename);
                smlDoc = myFile.ReadToEnd();
                myFile.Close();
                if (isSML(smlDoc))
                {
                    smlDoc = sml2xml(smlDoc);
                }
            }
            return smlDoc;
        }

        public static Stream translateStream(Stream instream)
        {
            string smlDoc = "";

            long spos = instream .Position ;
            System.IO.StreamReader myFile = new System.IO.StreamReader(instream);
            smlDoc = myFile.ReadToEnd();
            myFile.Close();
            //instream.Position = spos;

            if (isSML(smlDoc))
            {
                smlDoc = sml2xml(smlDoc);
            }

            byte[] byteArray = Encoding.ASCII.GetBytes(smlDoc);
            var stream = new MemoryStream(byteArray);
            return stream;
        }
    }

}