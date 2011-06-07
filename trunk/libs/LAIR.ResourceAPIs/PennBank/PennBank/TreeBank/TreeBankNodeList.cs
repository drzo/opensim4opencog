using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Linq;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// List of TreeBank nodes
    /// </summary>
    public class TreeBankNodeList : List<TreeBankNode>
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public TreeBankNodeList()
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="initialCapacity">Initial capacity of list</param>
        public TreeBankNodeList(int initialCapacity)
            : base(initialCapacity)
        {
        }
    }
}
