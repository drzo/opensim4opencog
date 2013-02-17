using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLbot.Utils;

namespace AltAIMLbot.Normalize
{
    /// <summary>
    /// Normalizes the input text into upper case
    /// </summary>
    public class MakeCaseInsensitive : TextTransformer
    {
        public MakeCaseInsensitive(AltBot bot, Unifiable inputString)
            : base(bot, null, inputString)
        { }

        public MakeCaseInsensitive(AltBot bot) : base(bot) 
        { }

        protected override Unifiable ProcessChangeU()
        {
            return this.InputStringU.ToCaseInsensitive();
        }
        protected override string ProcessChange()
        {
            return TransformInput(InputString);
        }
        /// <summary>
        /// An ease-of-use static method that re-produces the instance transformation methods
        /// </summary>
        /// <param name="input">The Unifiable to transform</param>
        /// <returns>The resulting Unifiable</returns>
        public static Unifiable TransformInput(Unifiable input)
        {
            return input.ToCaseInsensitive();
        }
        public static string TransformInput(string input)
        {
            input = ToUpper(input);
            return input;
        }
    }
}
