using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Inference.Proof;

    public interface IInferenceResult
    {
        /// <summary>
        /// 
        /// </summary>
        /// <returns>true, if the query is not entailed from the premises. This just means the query is not entailed, the query itself may be true.
        /// </returns>
        bool IsPossiblyFalse();
        
        /// <summary>
        /// 
        /// </summary>
        /// <returns>
        /// true, if the query is entailed from the premises (Note: can get partial results if the original 
        /// query contains variables indicating that there can possibly be more than 1 proof/bindings
        /// for the query, see: isPartialResultDueToTimeout()).
        /// </returns>
        bool IsTrue();

        /// <summary>
        /// 
        /// </summary>
        /// <returns>true, if the inference procedure ran for a length of time and found 
        /// no proof one way or the other before it timed out.</returns>
        bool IsUnknownDueToTimeout();

        /// <summary>
        /// 
        /// </summary>
        /// <returns>true, if the inference procedure found a proof for a query containing variables 
        /// (i.e. possibly more than 1 proof can be returned) and the inference procedure was still looking for 
        /// other possible answers before it timed out.</returns>
        bool IsPartialResultDueToTimeout();

        /// <summary>
        /// 
        /// </summary>
        /// <returns>a list of 0 or more proofs (multiple proofs can be returned if 
        /// the original query contains variables).</returns>
        IList<IProof> GetProofs();
    }
}
