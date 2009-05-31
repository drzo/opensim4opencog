using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The guardstar element tells the AIML interpreter that it should substitute the contents of a 
    /// wildcard from a pattern-side guard element. 
    /// 
    /// The guardstar element has an optional integer index attribute guard indicates which wildcard 
    /// to use; the minimum acceptable value for the index is "1" (the first wildcard). 
    /// 
    /// An AIML interpreter should raise an error if the index attribute of a star specifies a 
    /// wildcard that does not exist in the guard element's pattern content. Not specifying the index 
    /// is the same as specifying an index of "1". 
    /// 
    /// The guardstar element does not have any content. 
    /// </summary>
    public class guardstar : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query guard originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public guardstar(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "guardstar")
            {
                if (this.templateNode.Attributes.Count == 0)
                {
                    if (this.query.GuardStar.Count > 0)
                    {
                        return (Unifiable)this.query.GuardStar[0];
                    }
                    else
                    {
                        this.Proc.writeToLog("ERROR! An out of bounds index to guardstar was encountered when processing the input: " + this.request.rawInput);
                    }
                }
                else if (this.templateNode.Attributes.Count == 1)
                {
                    if (this.templateNode.Attributes[0].Name.ToLower() == "index")
                    {
                        if (this.templateNode.Attributes[0].Value.Length > 0)
                        {
                            try
                            {
                                int result = Convert.ToInt32(this.templateNode.Attributes[0].Value.Trim());
                                if (this.query.GuardStar.Count > 0)
                                {
                                    if (result > 0)
                                    {
                                        return (Unifiable)this.query.GuardStar[result - 1];
                                    }
                                    else
                                    {
                                        this.Proc.writeToLog("ERROR! An input tag with a bady formed index (" + this.templateNode.Attributes[0].Value + ") was encountered processing the input: " + this.request.rawInput);
                                    }
                                }
                                else
                                {
                                    this.Proc.writeToLog("ERROR! An out of bounds index to guardstar was encountered when processing the input: " + this.request.rawInput);
                                }
                            }
                            catch
                            {
                                this.Proc.writeToLog("ERROR! A guardstar tag with a bady formed index (" + this.templateNode.Attributes[0].Value + ") was encountered processing the input: " + this.request.rawInput);
                            }
                        }
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
