using System;
using System.Text;
using System.Xml;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The star element indicates that an AIML interpreter should substitute the value "captured" 
    /// by a particular wildcard from the pattern-specified portion of the match path when returning 
    /// the template. 
    /// 
    /// The star element has an optional integer index attribute that indicates which wildcard to use. 
    /// The minimum acceptable value for the index is "1" (the first wildcard), and the maximum 
    /// acceptable value is equal to the number of wildcards in the pattern. 
    /// 
    /// An AIML interpreter should raise an error if the index attribute of a star specifies a wildcard 
    /// that does not exist in the category element's pattern. Not specifying the index is the same as 
    /// specifying an index of "1". 
    /// 
    /// The star element does not have any content. 
    /// </summary>
    public class star : UnifibleTagHandler
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
        public star(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                return ((" " + templateNode.InnerText + " ").ToUpper().Equals(srch)) ? STAR_TRUE : STAR_FALSE;
            }
            if (templateNode.HasChildNodes)
            {
                {
                    Unifiable rest = with;
                    // recursively check
                    foreach (XmlNode childNode in templateNode.ChildNodes)
                    {

                        with = rest.First();
                        rest = rest.Rest();
                        try
                        {
                            if (childNode.NodeType == XmlNodeType.Text)
                            {
                                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                                return ((" " + childNode.InnerText + " ").ToUpper().Equals(srch)) ? STAR_TRUE : STAR_FALSE;
                            }
                            AIMLTagHandler part = Proc.GetTagHandler(user, query, request, result, childNode, this);
                            if (part.CanUnify(with)>0) return STAR_FALSE;
                        }
                        catch (Exception e)
                        {
                            RTPBot.writeDebugLine("" + e);
                        }
                    }
                    return rest.IsEmpty ? STAR_TRUE : STAR_FALSE;
                }
            }
            return with.IsEmpty ? STAR_TRUE : STAR_FALSE;
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "star")
            {
                if (this.query.InputStar.Count > 0)
                {
                    int index = Convert.ToInt32(GetAttribValue("index", "1"));
                    try
                    {
                        if (index <= query.InputStar.Count)
                        {
                            if (index > 0)
                            {
                                return (Unifiable)this.query.InputStar[index - 1];
                            }
                        }
                        else
                        {
                            writeToLogWarn("InputStar out of bounds reference caused by input: " + this.request.rawInput);
                        }
                    }
                    catch
                    {
                        writeToLogWarn("Index set to non-integer value whilst processing star tag in response to the input: " + this.request.rawInput);
                    }
                }
                else
                {
                    writeToLogWarn("A star tag tried to reference an empty InputStar collection when processing the input: " + this.request.rawInput);
                    return templateNodeInnerText;
                }
            }
            return Unifiable.Empty;
        }

    }
}
