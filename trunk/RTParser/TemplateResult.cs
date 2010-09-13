using System;
using RTParser.Utils;

namespace RTParser
{
    internal class TemplateResult : Exception
    {
        public bool TemplateSucceeded;
        public bool CreatedOutput;
        public string TemplateOutput;
        public AIMLTagHandler  TagHandler;
    }
}