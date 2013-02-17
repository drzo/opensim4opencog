using System;

namespace AltAIMLbot.Database
{
    public class TrueKnowledgeFactiodEngine
        : WebGetFactiodEngine
    {
        protected static string API_ACCT = "api_account_id=api_douglasmiles&api_password=it9ndxsw51d611to";
        public TrueKnowledgeFactiodEngine(IEnglishFactiodEngine fallback, AltBot AltBot)
            : base(fallback, AltBot)
        {
        }

        public override string GetResultTags()
        {
            return "tk:text_result,text_result,result";
        }

        protected override string MakeSearchString(string searchTerm1)
        {
            return
                @"https://api.trueknowledge.com/direct_answer?object_metadata=image64,logicmoo,api_douglasmiles,wikipedia,official&question=" +
                searchTerm1 + "&" + API_ACCT + "&retranslate=1";
        }

        public override string GetServiceName()
        {
            return "asktrue";
        }
    }
}