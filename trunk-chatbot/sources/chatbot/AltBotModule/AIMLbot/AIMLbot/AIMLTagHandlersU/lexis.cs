using System;
using System.Text.RegularExpressions;
using System.Xml;
using System.Text;
using System.Collections.Generic;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using RTParser.Utils;
// For Wordnet access
using LAIR.ResourceAPIs.WordNet;
using LAIR.Collections.Generic;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class lexis : UnifibleTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public lexis(RTParser.AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {

            string re = ComputeInner();

            string wnPos = GetAttribValue("wnpos", "").ToLower();
            string wnRelation = GetAttribValue("wnrel", "").ToLower();

            //Lookup definition for current word we could unify with
            string wordAttributes = "";
            string key = (string)with.ToValue(query).Trim();
            if (Proc.wordAttributeHash.Contains(key) )
            { 
                wordAttributes = (string)Proc.wordAttributeHash[key];
            }
            else
            {
                if (Proc.wordAttributeHash.Contains(key.ToLower()) )
                {
                    key = key.ToLower();
                    wordAttributes = (string)Proc.wordAttributeHash[key];
                }
            }
            // Can you find a match inside ?
            var matcher = new Regex(re);
            if (matcher.IsMatch(wordAttributes)) return AND_TRUE;


            // Ok, lets try WordNet
            WordNetEngine ourWordNetEngine = Proc.wordNetEngine;
            Set < SynSet > synPatternSet = null;

            // find our POS domain if possible
            WordNetEngine.POS ourPOS = WordNetEngine.POS.Noun;
            if (wnPos.Length > 0)
            {
                // populate POS list
                foreach (WordNetEngine.POS p in Enum.GetValues(typeof(WordNetEngine.POS)))
                    if (p != WordNetEngine.POS.None)
                    {
                        if (p.ToString().ToLower().Equals(wnPos) || p.ToString().ToLower().StartsWith(wnPos))
                        {
                            ourPOS = p;
                        }
                    }
            }
            if (ourWordNetEngine == null)
            {
                writeDebugLine("Wordnet engine not loaded .. returning AND_FALSE");
                {
                    return AND_FALSE;
                }
            }
            try { synPatternSet = ourWordNetEngine.GetSynSets(re, ourPOS); }
            catch (Exception)
            {
                return AND_FALSE;
            }
            if (synPatternSet.Count == 0)
            {
                try { synPatternSet = ourWordNetEngine.GetSynSets(re.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return AND_FALSE;
                }

            }

            Set<SynSet> synInputSet = null;
            try { synInputSet = ourWordNetEngine.GetSynSets(key, ourPOS); }
            catch (Exception)
            {
                return AND_FALSE;
            }
            if (synInputSet.Count == 0)
            {
                try { synInputSet = ourWordNetEngine.GetSynSets(key.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return AND_FALSE;
                }

            }

                List<WordNetEngine.SynSetRelation> vlist = new List<WordNetEngine.SynSetRelation>(); //[2];
                //vlist[0] = WordNetEngine.SynSetRelation.Hyponym;
                //vlist[1] = WordNetEngine.SynSetRelation.InstanceHyponym;
                //vlist[0] = WordNetEngine.SynSetRelation.Hypernym ;
                //vlist[1] = WordNetEngine.SynSetRelation.InstanceHypernym;
                if (wnRelation.Length == 0)
                {
                    vlist.Add(WordNetEngine.SynSetRelation.Hypernym);
                    vlist.Add(WordNetEngine.SynSetRelation.InstanceHypernym);
                }
                else
                {
                    // populate Relation list
                    foreach (WordNetEngine.SynSetRelation r in Enum.GetValues(typeof(WordNetEngine.SynSetRelation)))
                        if (r != WordNetEngine.SynSetRelation.None)
                        {
                            if (r.ToString().ToLower().Contains(wnRelation))
                            {
                                vlist.Add(r);
                            }
                        }

                }

            if ((synInputSet.Count > 0) && (synPatternSet.Count > 0))
            {
               foreach (SynSet synDstSet in synInputSet)
                {
                foreach (SynSet synSrcSet in synPatternSet)
                    {
                        //synSets.Items.Add(synSet);
                        List<SynSet> linkageList = null;

                        linkageList = synDstSet.GetShortestPathTo(synSrcSet, vlist);
                        if ((linkageList != null) && (linkageList.Count > 0))
                        {

                            return AND_TRUE;
                        }
                    }
                }
                return AND_FALSE;
            }

            return AND_FALSE;
        }

        protected override Unifiable ProcessChangeU()
        {
            var v1 = ComputeInner();
            var v2 = templateNodeInnerText;
            return v2;
        }
    }
}
