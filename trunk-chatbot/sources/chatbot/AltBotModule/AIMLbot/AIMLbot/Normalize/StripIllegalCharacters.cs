using System;
using System.Text.RegularExpressions;
using System.Text;
using AltAIMLbot.Utils;

namespace AltAIMLbot.Normalize
{
    /// <summary>
    /// Strips any illegal characters found within the input Unifiable. Illegal characters are referenced from
    /// the bot's Strippers regex that is defined in the setup XML file.
    /// </summary>
    public class StripIllegalCharacters : TextTransformer
    {
        public StripIllegalCharacters(AltBot bot, Unifiable inputString) : base(bot, null, inputString)
        { }

        public StripIllegalCharacters(AltBot bot)
            : base(bot) 
        { }
        protected override Unifiable ProcessChangeU()
        {
            return InputStringU;
            string newVariable = ((string)InputStringU);
            if (newVariable != null)
            {
                if (newVariable.StartsWith("TAG-")) return InputStringU;
            }
            return this.Proc.Strippers.Replace(this.InputStringU, " ");
        }
    }
}
