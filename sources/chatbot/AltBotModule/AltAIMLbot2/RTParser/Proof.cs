using System.Collections.Generic;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    public class Proof
    {
        /// <summary>
        /// Categories uses life a "proof"
        /// Also used ot prevent looping
        /// </summary>
        public readonly HashSet<object> UsedTemplates = new HashSet<object>();
        public bool Add(object node)
        {
            return UsedTemplates.Add(node);
        }

        public bool Contains(object node)
        {
            return UsedTemplates.Contains(node);
        }
    }
}