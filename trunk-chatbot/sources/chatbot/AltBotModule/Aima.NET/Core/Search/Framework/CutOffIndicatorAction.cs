using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent.Impl;

    /// <summary>
    /// A NoOp action that indicates a CutOff has occurred in a search. Used
    /// primarily by DepthLimited and IterativeDeepening search routines. 
    /// </summary>
    public class CutOffIndicatorAction : DynamicAction 
    {
        public static readonly CutOffIndicatorAction CutOff = new CutOffIndicatorAction();

        //
        // START-Action
        public override bool IsNoOp() {
            return true;
        }

        // END-Action
        //

        private CutOffIndicatorAction(): base("CutOff") 
        {
        }
    }
}
