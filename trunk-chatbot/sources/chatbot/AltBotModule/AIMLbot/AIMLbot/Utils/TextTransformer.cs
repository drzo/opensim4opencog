using System;

namespace AltAIMLbot.Utils
{
    /// <summary>
    ///   Encapsulates all the required methods and attributes for any text transformation.
    /// 
    ///   An input string is provided and various methods and attributes can be used to grab
    ///   a transformed string.
    /// 
    ///   The protected ProcessChange() method is abstract and should be overridden to contain 
    ///   the code for transforming the input text into the output text.
    /// </summary>
    public abstract class TextTransformer : StaticAIMLUtils
    {
        protected Unifiable _transformComplete;

        /// <summary>
        ///   The bot that this transformation is connected with
        /// </summary>
        public AltBot bot;

        public AltBot substs
        {
            get { return bot; }
            set
            {
                bot = value;//.bot;
            }
        }

        protected string initialString;

        /// <summary>
        ///   Instance of the input string
        /// </summary>
        protected Unifiable inputString;

        /// Instance of the input Unifiable
        /// </summary>
        protected Unifiable inputStringU;

        /// <summary>
        ///   ctor
        /// </summary>
        /// <param name="bot"> The bot this transformer is a part of </param>
        /// <param name="inputString"> The input string to be transformed </param>
        public TextTransformer(AltBot bot, string inputString)
            : this(bot, inputString, null)
        {
        }

        /// <summary>
        ///   ctor
        /// </summary>
        /// <param name="bot"> The bot this transformer is a part of </param>
        public TextTransformer(AltBot bot)
            : this(bot, string.Empty)
        {
        }

        /// <summary>
        ///   Default ctor for used as part of late binding mechanism
        /// </summary>
        public TextTransformer()
            : this(null, string.Empty)
        {
        }

        /// <summary>
        ///   ctor
        /// </summary>
        /// <param name="bot"> The bot this transformer is a part of </param>
        /// <param name="inu"> The input Unifiable to be transformed </param>
        public TextTransformer(AltBot bot, string instr, Unifiable inu)
        {
            this.bot = bot;
            inputStringU = inu ?? instr;
            inputString = initialString = instr ?? inputStringU.AsString();
        }

        /// <summary>
        ///   The input Unifiable to be transformed in some way
        /// </summary>
        public Unifiable InputStringU
        {
            get
            {
                if (inputStringU == null) throw new NullReferenceException("InputStringU");
                return inputStringU;
            }
            set
            {
                inputStringU = value;
                inputString = value.AsString();
                _transformComplete = null;
            }
        }

        /// <summary>
        ///   The transformed Unifiable
        /// </summary>
        public Unifiable OutputStringU
        {
            get { return Transform(); }
        }

        public AltBot Proc
        {
            get { return bot; }
        }

        /// <summary>
        ///   The input string to be transformed in some way
        /// </summary>
        public Unifiable InputString
        {
            get
            {
                if (inputString == null) throw new NullReferenceException("InputString");
                return inputString;
            }
            set
            {
                inputString = value;
                inputStringU = value;
                _transformComplete = null;
            }
        }

        /// <summary>
        ///   The transformed string
        /// </summary>
        public Unifiable OutputString
        {
            get { return Transform(); }
        }

        public virtual bool IsFormatter
        {
            get { return true; }
        }

        public override string ToString()
        {
            return GetType().Name + ": " + InputString;
        }

        /// <summary>
        ///   Do a transformation on the supplied input string
        /// </summary>
        /// <param name="input"> The string to be transformed </param>
        /// <returns> The resulting output </returns>
        public Unifiable TransformU(string input)
        {
            InputString = input;
            return Transform();
        }

        /// <summary>
        ///   Do a transformation on the string found in the InputString attribute
        /// </summary>
        /// <returns> The resulting transformed string </returns>
        public virtual Unifiable Transform()
        {
            if (_transformComplete != null) return _transformComplete;
            if (InputString.Length > 0)
            {
                if (_transformComplete == null)
                {
                    _transformComplete = ProcessChangeU();
                }
                return _transformComplete;
            }
            else
            {
                return string.Empty;
            }
        }

        /// <summary>
        ///   The method that does the actual processing of the text.
        /// </summary>
        /// <returns> The resulting processed text </returns>
        protected abstract Unifiable ProcessChangeU();

        public virtual float CallCanUnify(Unifiable with)
        {
            return InputStringU == with ? Unifiable.UNIFY_TRUE : Unifiable.UNIFY_FALSE;
        }
    }
}