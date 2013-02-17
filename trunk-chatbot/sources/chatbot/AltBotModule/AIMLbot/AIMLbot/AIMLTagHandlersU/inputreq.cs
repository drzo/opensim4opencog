using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
   /// <summary>
   ///  Returns to original text that <pattern> key'd from
   ///  <!--
   ///    INPUTREQ TESTS  
   ///  -->
   ///  <category>
   ///      <pattern>INPUTREQTEST1</pattern>
   ///      <template>This should say INPUTREQTEST1 for <inputreq/></template>
   ///  </category>
   ///  <category>
   ///      <pattern>INPUTREQTEST2</pattern>
   ///      <template><srai>INPUTREQTEST1<srai/></template>
   ///  </category>
   /// </summary>
    public class inputreq : AIMLTagHandlerU
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
        public inputreq(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }
        protected override bool ExpandingSearchWillYieldNoExtras { get { return true; } }
        protected override Unifiable ProcessChangeU()
        {
            if (CheckNode("inputreq"))
            {
                return request.rawInput;
            }
            return Unifiable.Empty;
        }
    }
}
