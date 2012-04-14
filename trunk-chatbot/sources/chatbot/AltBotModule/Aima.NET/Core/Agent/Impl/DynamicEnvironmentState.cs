// --------------------------------------------------------------------------------------------------------------------
// <copyright file="DynamicEnvironmentState.cs" company="">
//   
// </copyright>
// <summary>
//   Defines the DynamicEnvironmentState type.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace Aima.Core.Agent.Impl
{
    /// <summary>
    /// </summary>
    public class DynamicEnvironmentState : ObjectWithDynamicAttributes, IEnvironmentState
    {
        /// <summary>
        /// </summary>
        /// <returns>
        /// </returns>
        public override string DescribeType() 
        {
		    return this.GetType().Name;
	    }
    }
}
