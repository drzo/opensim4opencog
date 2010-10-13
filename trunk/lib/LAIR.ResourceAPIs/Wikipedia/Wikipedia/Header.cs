using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Header class, for information prior to the first section in the page
    /// </summary>
    public class Header : Section
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="headerLines">Lines for the header</param>
        /// <param name="containingPage">Page that contains this header</param>
        public Header(List<string> headerLines, Page containingPage)
            : base("Header", headerLines, null, containingPage)
        {
        }
    }
}
