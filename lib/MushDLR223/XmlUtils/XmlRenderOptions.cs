using System;
using System.Collections.Generic;
using System.Xml;

namespace MushDLR223.Utilities
{
    public class RenderOptions
    {
        public List<string> skip;
        public List<string> flatten;

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
}