using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    class filterqa : Utils.AIMLTagHandler
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
        public filterqa(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = true;
            this.isBoring = true;
        }
        protected override Unifiable ProcessChangeU()
        {
            // takes the contents returned, spliting into canidate answers using sep attribute
            // then sees if it is a failure or error
            // will return any response that is not an error/failure
            // if no anwere will report failure and error
            // also will trigger "onfail" and "onsuccess" behaviors


            string failurePhrase = "Sorry, I don't understand.";
            string failurePhrase2 = "Sorry. I couldn't understand what you are asking. Please rephrase.";
            string errorPhrase = "Processing caused the following error.";

            string sepToken ="ANSEP";
            try { 
                if (TemplateNodeAttributes["sep"] !=null)
                    sepToken = TemplateNodeAttributes["sep"].Value; }
            catch (Exception e) { }

            string onFail = null;
            try
            {
                if (TemplateNodeAttributes["onfail"] != null)
                    onFail = TemplateNodeAttributes["onfail"].Value; }
            catch (Exception e) { }

            string onSuccess = null;
            try {
                if (TemplateNodeAttributes["onsuccess"] != null)
                    onSuccess = TemplateNodeAttributes["onsuccess"].Value;
            }
            catch (Exception e) { }

            if (this.TemplateNodeName == "filterqa")
            {
                //string message = this.templateNode.InnerText ;
                string message = this.TemplateNodeInnerText;
                //if (this.TemplateNodeHasText)
                    if (message.Length >0)
                    {

                    string valid = "";
                    string errorMessage = "";
                    string failMessage = "";
                    string [] fragments = message.Split(new string[] {sepToken},StringSplitOptions.None);


                    bool noAnswer = true;
                    foreach (string canidateC in fragments)
                    {
                        string canidate = canidateC.Trim();
                        if (canidate.Length == 0) continue;
                        bool failureFlag = canidate.Contains(failurePhrase) || canidate.Contains(failurePhrase2);
                        bool errorFlag = canidate.Contains(errorPhrase);
                        if ((!failureFlag) && (!errorFlag))
                        {
                            if (!valid.Contains(canidate))
                            {
                                valid += canidate + "\n";
                            }
                            noAnswer = false;
                        }
                        else
                        {
                            if (failureFlag) failMessage = canidate;
                            if (errorFlag) errorMessage += canidate+"\n";
                        }
                    }
                    if (noAnswer == false)
                    {
                        if (onSuccess != null)
                        {
                            bot.myBehaviors.queueEvent(onSuccess);
                        }
                        /*
                        if (valid.Length > 256)
                        {
                            TokenRanker myRanker = new TokenRanker();
                            myRanker.defineRank(valid);
                            string myRankSummary = myRanker.summaryByRank(512);
                            string mySeqSummary = myRanker.summaryByOriginalSequence(512);
                            return mySeqSummary;
                        }
                        else
                        {
                            return valid;
                        }
                        */
                        return valid;
                    }
                    else
                    {
                        return failMessage + " " + errorMessage;
                    }
                }
            }

            if (onFail != null)
            {
                bot.myBehaviors.queueEvent(onFail);
            }
            return failurePhrase;
        }

    }
}
