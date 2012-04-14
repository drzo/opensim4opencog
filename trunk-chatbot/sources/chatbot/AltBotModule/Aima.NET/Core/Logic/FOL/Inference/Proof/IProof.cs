using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference.Proof
{
    using Aima.Core.Logic.FOL.Parsing.AST;

    public interface IProof
    {
        /// <summary>
        /// 
        /// </summary>
        /// <returns>A list of proof steps that show how an answer was derived.</returns>
        IList<IProofStep> GetSteps();

        /// <summary>
        /// 
        /// </summary>
        /// <returns>a Map of bindings for any variables that were in the original query. 
        /// Will be an empty Map if no variables existed in the original query.</returns>
        IDictionary<Variable, ITerm> GetAnswerBindings();

        /// <summary>
        /// 
        /// </summary>
        /// <param name="updatedBindings">allows for the bindings to be renamed. 
        /// Note: should not be used for any other reason.</param>
        void ReplaceAnswerBindings(IDictionary<Variable, ITerm> updatedBindings);
    }
}
