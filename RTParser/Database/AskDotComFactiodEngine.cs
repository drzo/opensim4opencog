using System;

namespace RTParser.Database
{
    class AskDotComFactiodEngine : WebGetFactiodEngine
    {
        public AskDotComFactiodEngine(IEnglishFactiodEngine fallback, RTPBot rtpBot) : base(fallback, rtpBot)
        {
        }

        public override string GetResultTags()
        {
            return "q_result,id=result-table,query_result,result,q_answer";
        }

        protected override string MakeSearchString(string searchTerm1)
        {
            return @"http://www.ask.com/web?q=" + searchTerm1;
        }

        public override string GetServiceName()
        {
            return "askask";
        }
    }
}