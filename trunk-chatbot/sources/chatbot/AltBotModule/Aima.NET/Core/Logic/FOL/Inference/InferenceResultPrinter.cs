using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.Inference.Proof;

    public class InferenceResultPrinter
    {
        /// <summary>
        /// Utility method for outputting InferenceResults in a formatted textual
        /// representation.
        /// </summary>
        /// <param name="ir">an InferenceResult</param>
        /// <returns>a String representation of the InferenceResult.</returns>
        public static string PrintInferenceResult(IInferenceResult ir)
        {
            var sb = new StringBuilder();

            sb.Append("InferenceResult.isTrue=" + ir.IsTrue());
            sb.Append("\n");
            sb.Append("InferenceResult.isPossiblyFalse=" + ir.IsPossiblyFalse());
            sb.Append("\n");
            sb.Append("InferenceResult.isUnknownDueToTimeout=" + ir.IsUnknownDueToTimeout());
            sb.Append("\n");
            sb.Append("InferenceResult.isPartialResultDueToTimeout=" + ir.IsPartialResultDueToTimeout());
            sb.Append("\n");
            sb.Append("InferenceResult.#Proofs=" + ir.GetProofs().Count);
            sb.Append("\n");
            int proofNo = 0;
            foreach (var p in ir.GetProofs())
            {
                proofNo++;
                sb.Append("InferenceResult.Proof#" + proofNo + "=\n" + ProofPrinter.PrintProof(p));
            }

            return sb.ToString();
        }
    }
}
