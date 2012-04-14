// --------------------------------------------------------------------------------------------------------------------
// <copyright file="DynamicState.cs" company="">
//   
// </copyright>
// <summary>
//   The dynamic state.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace Aima.Core.Agent.Impl
{
    /// <summary>
    /// The dynamic state.
    /// </summary>
    public class DynamicState : ObjectWithDynamicAttributes, IState
    {
        #region Public Methods

        /// <summary>
        /// The describe type.
        /// </summary>
        /// <returns>
        /// The describe type.
        /// </returns>
        public override string DescribeType()
        {
            return this.GetType().Name;
        }

        #endregion
    }
}