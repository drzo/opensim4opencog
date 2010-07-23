using System;
using System.Text;
using System.Xml;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The template-side that element indicates that an AIML interpreter should substitute the 
    /// contents of a previous bot output. 
    /// 
    /// The template-side that has an optional index attribute that may contain either a single 
    /// integer or a comma-separated pair of integers. The minimum value for either of the integers 
    /// in the index is "1". The index tells the AIML interpreter which previous bot output should be 
    /// returned (first dimension), and optionally which "sentence" (see [8.3.2.]) of the previous bot
    /// output (second dimension). 
    /// 
    /// The AIML interpreter should raise an error if either of the specified index dimensions is 
    /// invalid at run-time. 
    /// 
    /// An unspecified index is the equivalent of "1,1". An unspecified second dimension of the index 
    /// is the equivalent of specifying a "1" for the second dimension. 
    /// 
    /// The template-side that element does not have any content. 
    /// </summary>
    public class that : RTParser.Utils.AIMLTagHandler
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
        public that(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "that")
            {
                if (this.templateNode.Attributes.Count == 0)
                {
                    return this.user.getThat();
                }
                //else if (this.templateNode.Attributes.Count == 1)
                {
                    var at1 = GetAttribValue("index", null);//.Trim();
                    if (at1 != null)
                    {
                        if (at1.Length > 0)
                        {
                            try
                            {
                                // see if there is a split
                                string[] dimensions = at1.Split(",".ToCharArray());
                                if (dimensions.Length == 2)
                                {
                                    int result = Convert.ToInt32(dimensions[0].Trim());
                                    int sentence = Convert.ToInt32(dimensions[1].Trim());
                                    if ((result > 0) & (sentence > 0))
                                    {
                                        return this.user.getThat(result - 1, sentence - 1);
                                    }
                                    else
                                    {
                                        writeToLogWarn("ERROR! An input tag with a bady formed index (" + at1 + ") was encountered processing the input: " + this.request.rawInput);
                                    }
                                }
                                else
                                {
                                    int result = Convert.ToInt32(at1.Trim());
                                    if (result > 0)
                                    {
                                        return this.user.getThat(result - 1);
                                    }
                                    else
                                    {
                                        writeToLogWarn("ERROR! An input tag with a bady formed index (" + at1 + ") was encountered processing the input: " + this.request.rawInput);
                                    }
                                }
                            }
                            catch (Exception exception)
                            {
                                writeToLogWarn("ERROR! An input tag with a bady formed index (" 
                                    + at1 + ") was encountered processing the input: " + this.request.rawInput + " " + exception);
                            }
                        }
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
