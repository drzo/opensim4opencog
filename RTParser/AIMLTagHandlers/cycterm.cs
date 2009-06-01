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
            //isRecursive = false;
        }


        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycterm")
            {                    
                Unifiable filter = base.GetAttribValue("filter", GetAttribValue("isa", "Thing"));
                Unifiable r = Recurse();
                Unifiable term;
                if (lookup(r, filter, out term))
                {
                    return term;
                }
                return Unifiable.Empty;
            }
            return Unifiable.Empty;
        }

        private bool lookup(Unifiable text,Unifiable filter,out Unifiable term)
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
            filter = Proc.Cyclify(filter);
            Unifiable ptext = text.ToPropper();
            if(false
            || lookupCycTerm("(#$nameString ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$denotation #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter, out term)
            || lookupCycTerm("(#$denotationRelatedTo #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter,out term)
            || lookupCycTerm("(#$initialismString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$abbreviationString-PN ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredNameString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-LongForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-ShortForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$acronymString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$scientificName ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$termStrings ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$termStrings-GuessedFromName ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$prettyString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$nicknames ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredTermStrings ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-Strict)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-General)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || lookupCycTerm("(#$preferredGenUnit ?CYCOBJECT ?POS #$%s-TheWord )", ptext, filter, out term)
            || lookupCycTerm("(#$and (#$wordStrings ?WORD \"%s\") (#$or (#$denotation ?WORD ?TEXT ?TYPE ?CYCOBJECT) (#$denotationRelatedTo ?WORD ?TEXT ?TYPE ?CYCOBJECT) ))", text, filter,out term))            
                return true;
            term = this.Proc.EvalSubL(String.Format("(car (fi-complete \"{0}\"))", text),null);
            // Followed by asking Cyc to guess at the word using (fi-complete \”%s\”)
            if (Unifiable.IsTrue(term))
            {
                if (Proc.IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = this.Proc.EvalSubL(String.Format("(cdr (car (denotation-mapper \"{0}\")))", text),null);
            if (Unifiable.IsTrue(term))
            {
                if (this.Proc.IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = this.Proc.EvalSubL(String.Format("(car (denots-of-Unifiable \"{0}\"))", text), null);
            if (Unifiable.IsTrue(term))
            {
                if (this.Proc.IsaFilter(term, filter))
                {
                    return true;
                }
            }
            // and if that fails returns a Unifiable of using #$\”%s\”
            term = Unifiable.Format("#${0}", text);
            return false;
        }

        //(mapcar #'(lambda (x) (pwhen (member col x) ))  (denotation-mapper "isa"))

        private bool lookupCycTerm(Unifiable template, Unifiable text,Unifiable filter, out Unifiable term)
        {
            template = template.Replace("%s", text);            
            try
            {
	            term = this.Proc.EvalSubL(String.Format("(cdr (assoc '?CYCOBJECT (nth 0 (cyc-query '(#$and {0} (#$isa ?CYCOBJECT {1})) #$EverythingPSC )) )))", template,filter), null);
            }
            catch (System.Exception ex)
            {
                term = null;
                return false;
            }
            if (Unifiable.IsFalse(term)) return false;
            return true;
        }
    }
}
