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
        public cycterm(RTParser.RTPBot bot,
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
                string filter = base.GetAttribValue("filter", GetAttribValue("isa", "Thing"));
                string term;
                if (lookup(Recurse(), filter, out term)) return term;
            }
            return string.Empty;
        }

        private bool lookup(string text,string filter,out string term)
        {
            if (!Proc.CycEnabled)
            {
                term = String.Format("\"{0}\"", text);
                return true;
            }
            if (text.Length<2)
            {
                term = text;
                return false;
            }
            string ptext = text.Substring(0, 1).ToUpper() + text.Substring(1);
            if(false
            || lookupCycTerm("(cyc-query '(#$nameString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter, out term)
            || lookupCycTerm("(cyc-query '(#$denotation #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT) #$EverythingPSC)", ptext, filter, out term)
            || lookupCycTerm("(cyc-query '(#$denotationRelatedTo #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT) #$EverythingPSC)", ptext, filter,out term)
            || lookupCycTerm("(cyc-query '(#$initialismString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$abbreviationString-PN ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$preferredNameString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$countryName-LongForm ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$countryName-ShortForm ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$acronymString ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$scientificName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$termStrings ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$termStrings-GuessedFromName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$prettyName ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$nicknames ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$preferredTermStrings ?CYCOBJECT \"%s\") #$EverythingPSC)", text, filter,out term)
            || lookupCycTerm("(cyc-query '(#$preferredGenUnit ?CYCOBJECT ?POS #$%s-TheWord ) #$EverythingPSC)", ptext, filter,out term)
            || lookupCycTerm("(cyc-query '(#$and (#$wordStrings ?WORD \"%s\") (#$or (#$denotation ?WORD ?TEXT ?TYPE ?CYCOBJECT) (#$denotationRelatedTo ?WORD ?TEXT ?TYPE ?CYCOBJECT) )) #$EverythingPSC)", text, filter,out term))            
                return true;
            term = this.Proc.EvalSubL(String.Format("(car (fi-complete \"{0}\"))", text),null);
            // Followed by asking Cyc to guess at the word using (fi-complete \”%s\”)
            if (!String.IsNullOrEmpty(term) && term.ToUpper() != "NIL")
            {
                if (Proc.IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = this.Proc.EvalSubL(String.Format("(cdr (car (denotation-mapper \"{0}\")))", text),null);
            if (!String.IsNullOrEmpty(term) && term.ToUpper() != "NIL")
            {
                if (this.Proc.IsaFilter(term, filter))
                {
                    return true;
                }
            }
            // and if that fails returns a string of using #$\”%s\”
            term = string.Format("#${0}", text);
            return false;
        }

        //(mapcar #'(lambda (x) (pwhen (member col x) ))  (denotation-mapper "isa"))

        private bool lookupCycTerm(string template, string text,string filter, out string term)
        {
            template = template.Replace("%s", text);
            term = this.Proc.EvalSubL(String.Format("(cdr (assoc '?CYCOBJECT (nth 0 {0})))", template), null);
            if (String.IsNullOrEmpty(term) || term.ToUpper() == "NIL") return false;
            return this.Proc.IsaFilter(term,filter);
        }
    }
}
