using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;

namespace AltAIMLbot.AIMLTagHandlers
{
    class filterqa : AltAIMLbot.Utils.AIMLTagHandler
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
        public filterqa(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = true;
        }
        protected override string ProcessChange()
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
                if (this.templateNode.Attributes["sep"] !=null)
                    sepToken = this.templateNode.Attributes["sep"].Value; }
            catch (Exception e) { }

            string onFail = null;
            try
            {
                if (this.templateNode.Attributes["onfail"] != null)
                    onFail = this.templateNode.Attributes["onfail"].Value; }
            catch (Exception e) { }

            string onSuccess = null;
            try {
                if (this.templateNode.Attributes["onsuccess"] != null)
                    onSuccess = this.templateNode.Attributes["onsuccess"].Value;
            }
            catch (Exception e) { }

            if (this.templateNode.Name.ToLower() == "filterqa")
            {
                if (this.templateNode.InnerText.Length > 0)
                {

                    string message = this.templateNode.InnerText;
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
                        if (valid.Length > 512)
                        {
                            TokenRanker myRanker = new TokenRanker();
                            myRanker.defineRank(valid);
                            string myRankSummary = myRanker.summaryByRank(512);
                            string mySeqSummary = myRanker.summaryByOriginalSequence(512);
                            return myRankSummary;
                        }
                        else
                        {
                            return valid;
                        }
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
