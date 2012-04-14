using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    public class ProofPrinter
    {
        /// <summary>
        /// Utility method for outputting proofs in a formatted textual representation.
        /// </summary>
        /// <param name="aProof"></param>
        /// <returns>a string representation of the Proof.</returns>
        public static string PrintProof(IProof aProof) 
        {
            StringBuilder sb = new StringBuilder();

            sb.Append("Proof, Answer Bindings: ");
            sb.Append(aProof.GetAnswerBindings());
            sb.Append("\n");

            IList<IProofStep> steps = aProof.GetSteps();

            int maxStepWidth = "Step".Length;
            int maxProofWidth = "Proof".Length;
            int maxJustificationWidth = "Justification".Length;

            // Calculate the maximum width for each column in the proof
            foreach (IProofStep step in steps) 
            {
                string sn = "" + step.GetStepNumber();
                if (sn.Length > maxStepWidth)
                {
                    maxStepWidth = sn.Length;
                }
                if (step.GetProof().Length > maxProofWidth) {
                    maxProofWidth = step.GetProof().Length;
                }
                if (step.GetJustification().Length > maxJustificationWidth) {
                    maxJustificationWidth = step.GetJustification().Length;
                }
            }

            // Give a little extra padding
            maxStepWidth += 1;
            maxProofWidth += 1;
            maxJustificationWidth += 1;

            //string f = "|%-" + maxStepWidth + "s| %-" + maxProofWidth + "s|%-"
            //        + maxJustificationWidth + "s|\n";

            string f = "|{0}-" + maxStepWidth + "s| {1}-" + maxProofWidth + "s|{2}-"
                    + maxJustificationWidth + "s|\n";

            int barWidth = 5 + maxStepWidth + maxProofWidth + maxJustificationWidth;
            var bar = new StringBuilder();
            for (int i = 0; i < barWidth; i++) {
                bar.Append("-");
            }
            bar.Append("\n");

            sb.Append(bar);
            sb.Append(string.Format(f, "Step", "Proof", "Justification"));
            sb.Append(bar);
            foreach (IProofStep step in steps) 
            {
                sb.Append(string.Format(f, "" + step.GetStepNumber(), step
                        .GetProof(), step.GetJustification()));
            }
            sb.Append(bar);

            return sb.ToString();
        }
    }

}
