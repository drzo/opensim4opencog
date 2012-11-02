using System;
using System.Collections;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace RTParser.Database
{
    public interface IEnglishFactiodEngine
    {
        string AskQuery(string searchTerm1, OutputDelegate dbgLog, Func<Unifiable> OnFalure,
                        XmlNode templateNode, float threshold, bool expandWithWordNet, bool expandOnNoHits,
                        out float reliablity);

        int InsertFactiod(string myText, XmlNode templateNode, WordExpander WordNetExpand);
        /// <summary>
        /// This method searches for the search query, then deletes the top ranked and inserts.
        /// </summary>
        /// <param name="query">The search term as a string that the caller wants to search for within the
        /// index as referenced by this object.</param>
        /// <param name="myText">The new value to replace in the database.</param>
        int UpdateFactoid(string searchQuery, string myText, XmlNode templateNodeOnNull);
        string MayPush(string text, XmlNode templateNodeOnNull);
        string MayAsk(string text, XmlNode templateNodeOnNull);
        int DeleteTopScoring(string myText, XmlNode templateNode, bool mustContainExact);
        long LoadDocuments(string file, XmlNode templateNode);
        bool IsDbPresent { get; }
        string FixPronouns(string myText, Func<string, string> getOrNullIfMissing);
    }
}