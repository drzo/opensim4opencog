using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using UPath = AltAIMLbot.Unifiable;


namespace AltAIMLbot
{

    public class RTPBot : AltBot
    {

    }

    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public static class HelperForMerge
    {


        public static string TrimIC(this IComparable<string> s, params char[] iC)
        {
            if (s == null) return "";
            if (s is Unifiable)
            {
                string us = ((Unifiable) s).AUnifyString;
                return us.Trim(iC);
            }
            else
            {
                string us = "" + s;
                return us.Trim(iC);
            }
        }


        public static string WhyBadNode(this XmlNode templateNode)
        {
            string why = "";
            templateNode.WhyBadNode((s) => { why += s; });
            return why;
        }

        public static void CheckNode(this XmlNode templateNode, bool throwIfBad)
        {
            var ox = templateNode.OuterXml;
            string why = "";
            templateNode.WhyBadNode((s) =>
                                        {
                                            DLRConsole.DebugWriteLine(ox + "" + s);
                                            if (throwIfBad) why += s;
                                        });
            if (throwIfBad && !string.IsNullOrEmpty(why)) throw new XmlException(templateNode + " bad = " + why);
        }

        public static void WhyBadNode(this XmlNode templateNode, Action<string> why)
        {
            if (!templateNode.HasChildNodes) return;
            int textChilds = 0;
            XmlNodeType lastType = XmlNodeType.Element;
            int commentCount = 0;
            int elementCount = 0;
            XmlNode lastChild = null;
            foreach (XmlNode c in templateNode)
            {
                if (c is XmlElement)
                {
                    elementCount++;
                    continue;
                }
                if (c is XmlComment)
                {
                    commentCount++;
                }
                if (c is XmlText)
                {
                    textChilds++;
                }
                if (lastChild != null && c.NodeType == lastType)
                {
                    why("two nodes side by side " + lastType);
                }
                WhyBadNode(c, why);
                lastChild = c;
            }
            if (elementCount != 0)
            {
                return;
            }
            if (elementCount == 0 && textChilds > 1)
            {
                why("two textChilds " + templateNode);
            }
        }

        public static bool HasChildNodesNonText(this XmlNode templateNode)
        {
            templateNode.CheckNode(true);
            foreach (XmlNode c in templateNode)
            {
                if (c is XmlElement)
                {
                    return true;
                }
            }
            return false;
        }

        public static int IsOneOf(this string graphPath, params string[] ces)
        {
            bool gpnull = graphPath == null;
            string ew = gpnull ? null : graphPath.ToLower();
            for (int index = 0; index < ces.Length; index++)
            {
                var ce = ces[index];
                if (ce == null)
                {
                    if (gpnull) return index;
                    continue;
                }
                if (ce.Equals(ew)) return index;
            }
            if (gpnull) return -2;
            return -1;
        }

        public static int ContainsAny(this string graphPath, params string[] ces)
        {
            bool gpnull = string.IsNullOrEmpty(graphPath);
            string ew = gpnull ? "" : graphPath.ToLower();
            for (int index = 0; index < ces.Length; index++)
            {
                var ce = ces[index];
                if (string.IsNullOrEmpty(ce))
                {
                    if (gpnull) return index;
                    continue;
                }
                if (!gpnull && ew.Contains(ce.ToLower()))
                {
                    return index;
                }
            }
            if (gpnull) return -2;
            return -1;
        }

        public static string RemoveEnd(this string graphPath, params string[] ces)
        {
            if (graphPath == null) return null;
            var ew = graphPath.ToLower();
            int len = graphPath.Length;
            foreach (var ce in ces)
            {
                int cel = ce.Length;
                if (len >= cel)
                {
                    if (ew.EndsWith(ce))
                    {
                        graphPath = graphPath.Substring(0, len - cel);
                        ew = graphPath.ToLower();
                        len = graphPath.Length;
                    }
                }
            }
            return graphPath;
        }
    }
}