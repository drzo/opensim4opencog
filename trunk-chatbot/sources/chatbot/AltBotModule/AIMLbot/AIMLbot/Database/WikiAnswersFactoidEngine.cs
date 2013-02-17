namespace AltAIMLbot.Database
{
    class WikiAnswersFactoidEngine : WebGetFactiodEngine
    {
        public WikiAnswersFactoidEngine(IEnglishFactiodEngine fallback, AltBot AltBot) : base(fallback, AltBot)
        {
        }

        public override string GetResultTags()
        {
            return "id=q_answer";
        }

        protected override string MakeSearchString(string searchTerm1)
        {
            return "http://wiki.answers.com/Q/" + searchTerm1.Replace("%20", "_");
        }
        public override string GetServiceName()
        {
            return "askanswer";
        }
    }
}