using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycterm&gt; translates an English word/phrase into a Cyc symbol 
    /// </summary>
    public class cycterm : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public cycterm(RTParser.Bot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }


        protected override string ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycterm")
            {
                if (templateNodeInnerText.Length > 0)
                {
                    string filter = base.GetAttribValue("filter");
                    return lookup(Recurse(), filter);
                }
            }
            return string.Empty;
        }

        private string lookup(string text,string filter)
        {
            string term;
            if(lookupCycTerm("(fi-ask '(#$denotation #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT) #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$denotationRelatedTo #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT) #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$nameString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$initialismString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$abbreviationString-PN ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$preferredNameString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$countryName-LongForm ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$countryName-ShortForm ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$acronymString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$scientificName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$termStrings ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$termStrings-GuessedFromName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$prettyName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$nicknames ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$preferredTermStrings ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$preferredGenUnit ?CYCOBJECT ?POS #$%s-TheWord ) #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(fi-ask '(#$and (#$wordStrings ?WORD \"%s\") (#$or (#$denotation ?WORD ?TEXT ?TYPE ?CYCOBJECT) (#$denotationRelatedTo ?WORD ?TEXT ?TYPE ?CYCOBJECT) )) #$EverythingPSC)", text, filter,out term))            
                return term;
            term = this.bot.EvalSubL(String.Format("(car (fi-complete \"{0}\"))", text),null);
            // Followed by asking Cyc to guess at the word using (fi-complete \”%s\”)
            if (term.ToUpper() != "NIL")
            {
                if (this.bot.IsaFilter(term, filter)) return term;
            }
            // and if that fails returns a string of using #$\”%s\”
            return string.Format("#${0}", text);
        }

        private bool lookupCycTerm(string template, string text,string filter, out string term)
        {
            template = template.Replace("%s", text);
            term = this.bot.EvalSubL("(cdr (assoc '?CYCOBJECT (nth 0 " + template + ")))", null);
            if (term.ToUpper() == "NIL") return false;
            return this.bot.IsaFilter(term,filter);
        }
    }
}
