using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Logic.FOL.Domain
{
    public class FOLDomainAnswerLiteralAddedEventArgs :EventArgs
    {
        public string AnswerLiteralName { get; private set; }

        public FOLDomainAnswerLiteralAddedEventArgs(string answerLiteralName) 
        {
            this.AnswerLiteralName = answerLiteralName;
        }
    }
}