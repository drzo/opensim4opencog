using System;
using System.Collections.Generic;
using System.Text;

namespace RTParser.Normalize
{
    /// <summary>
    /// Normalizes the input text into upper case
    /// </summary>
    public class MakeCaseInsensitive : RTParser.Utils.TextTransformer
    {
        public MakeCaseInsensitive(RTParser.AltBot bot, Unifiable inputString)
            : base(bot, null, inputString)
        { }

        public MakeCaseInsensitive(RTParser.AltBot bot) : base(bot) 
        { }

        protected override Unifiable ProcessChangeU()
        {
            return this.InputStringUU.ToCaseInsensitive();
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
