using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public class Proof
    {
        public readonly HashSet<object> UsedTemplates =new HashSet<object>();
        /// <summary>
        /// Categories uses life a "proof"
        /// Also used ot prevent looping
        /// </summary>
        public readonly HashSet<object> Proof1 = new HashSet<object>();
        /// <summary>
        /// Also used ot prevent looping
        /// </summary>
        public readonly HashSet<object> Proof2 = new HashSet<object>();

        public bool Add(Node node)
        {
            if (!Proof1.Add(node))
            {
                return false;
                if (!Proof2.Add(node))
                {
                    return false;
                }
            }
            return true;
        }

        public bool Add(TemplateInfo node)
        {
            return UsedTemplates.Add(node);
        }
        public bool Contains(TemplateInfo node)
        {
            return UsedTemplates.Contains(node);
        }
    }
}