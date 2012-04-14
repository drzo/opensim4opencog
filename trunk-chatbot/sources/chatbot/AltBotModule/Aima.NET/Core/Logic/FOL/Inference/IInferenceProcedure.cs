using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Inference
{
    using Aima.Core.Logic.FOL.KB;
    using Aima.Core.Logic.FOL.Parsing.AST;

    public interface IInferenceProcedure
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="kb">the knowledge base against which the query is to be made.</param>
        /// <param name="aQuery">to be answered.</param>
        /// <returns>an IInferenceResult.</returns>
        IInferenceResult Ask(FOLKnowledgeBase kb, ISentence aQuery);

    }
}
