// --------------------------------------------------------------------------------------------------------------------
// <copyright file="NoOpAction.cs" company="">
//   
// </copyright>
// <summary>
//   The no op action.
// </summary>
// --------------------------------------------------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Agent.Impl
{
    /// <summary>
    /// The no op action.
    /// </summary>
    public class NoOpAction : DynamicAction
    {
        /// <summary>
        /// The no op.
        /// </summary>
        public static readonly NoOpAction NoOp = new NoOpAction();

        /// <summary>
        /// The is no op.
        /// </summary>
        /// <returns>
        /// The is no op.
        /// </returns>
        public new bool IsNoOp()
        {
            return true;
        }

        /// <summary>
        /// Prevents a default instance of the <see cref="NoOpAction"/> class from being created.
        /// </summary>
        private NoOpAction()
            : base("NoOp")
        {
        }
    }
}