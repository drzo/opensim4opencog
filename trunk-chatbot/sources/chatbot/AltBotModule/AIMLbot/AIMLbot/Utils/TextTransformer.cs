using System;
using java.lang;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// Encapsulates all the required methods and attributes for any text transformation.
    /// 
    /// An input string is provided and various methods and attributes can be used to grab
    /// a transformed string.
    /// 
    /// The protected ProcessChange() method is abstract and should be overridden to contain 
    /// the code for transforming the input text into the output text.
    /// </summary>
    abstract public class TextTransformer : StaticAIMLUtils
    {

        public override string ToString()
        {
            return GetType().Name + ": " + InputString;
        }

        #region Attributes
        /// <summary>
        /// Instance of the input string
        /// </summary>
        protected string inputString;

        /// <summary>
        /// The bot that this transformation is connected with
        /// </summary>
        public AltBot bot;

        public AltBot Proc
        {
            get { return bot; }
        }

        /// <summary>
        /// The input string to be transformed in some way
        /// </summary>
        public string InputString
        {
            get
            {
                if (inputString == null) throw new NullPointerException("InputString");
                return this.inputString;
            }
            set
            {
                this.inputString = value;
                inputStringU = value;
                transformComplete = null;
            }
        }

        /// <summary>
        /// The transformed string
        /// </summary>
        public string OutputString
        {
            get{return this.Transform();}
        }

        public virtual bool isFormatter
        {
            get { return true; }
        }

        #endregion

        /// <summary>
        /// ctor
        /// </summary>
        /// <param name="bot">The bot this transformer is a part of</param>
        /// <param name="inputString">The input string to be transformed</param>
        public TextTransformer(AltBot bot, string inputString)
            : this(bot, inputString, null)
        {
        }

        /// <summary>
        /// ctor
        /// </summary>
        /// <param name="bot">The bot this transformer is a part of</param>
        public TextTransformer(AltBot bot)
            : this(bot, string.Empty)
        {
        }

        /// <summary>
        /// Default ctor for used as part of late binding mechanism
        /// </summary>
        public TextTransformer()
            : this(null, string.Empty)
        {
        }

        /// <summary>
        /// Do a transformation on the supplied input string
        /// </summary>
        /// <param name="input">The string to be transformed</param>
        /// <returns>The resulting output</returns>
        public string Transform(string input)
        {
            this.InputString = input;
            return this.Transform();
        }

        private string transformComplete = null;
        /// <summary>
        /// Do a transformation on the string found in the InputString attribute
        /// </summary>
        /// <returns>The resulting transformed string</returns>
        virtual public string Transform()
        {
            if (InputString.Length > 0)
            {
                if (transformComplete == null)
                {
                    transformComplete = this.ProcessChange();
                }
                return transformComplete;
            }
            else
            {
                return string.Empty;
            }
        }

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected abstract string ProcessChange();

        /// <summary>
        /// ctor
        /// </summary>
        /// <param name="bot">The bot this transformer is a part of</param>
        /// <param name="inu">The input Unifiable to be transformed</param>
        public TextTransformer(AltBot bot, string instr, Unifiable inu)
        {
            this.bot = bot;
            this.inputStringU = inu ?? instr;
            this.inputString = initialString = instr ?? inputStringU.AsString();
        }

        public virtual float CallCanUnify(Unifiable with)
        {
            return InputStringU == with ? Unifiable.UNIFY_TRUE : Unifiable.UNIFY_FALSE;
        }

        /// <summary>
        /// Do a transformation on the supplied input Unifiable
        /// </summary>
        /// <param name="input">The Unifiable to be transformed</param>
        /// <returns>The resulting output</returns>
        public Unifiable TransformU(Unifiable input)
        {
            this.InputStringU = input;
            return this.TransformU();
        }

        /// <summary>
        /// Do a transformation on the Unifiable found in the InputString attribute
        /// </summary>
        /// <returns>The resulting transformed Unifiable</returns>
        public virtual Unifiable TransformU()
        {
            if (!IsNullOrEmpty(this.InputStringU))
            {
                return this.ProcessAimlChange();
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        public virtual Unifiable ProcessAimlChange()
        {
            return ProcessChangeU();
        }

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected abstract Unifiable ProcessChangeU();

        public virtual Unifiable CompleteProcessU()
        {
            return InputStringU;
        }

        #region Attributes

        public string initialString;

        /// <summary>
        /// Instance of the input Unifiable
        /// </summary>
        protected Unifiable inputStringU;


        /// <summary>
        /// The input Unifiable to be transformed in some way
        /// </summary>
        public Unifiable InputStringU
        {
            get
            {
                if (inputStringU == null) throw new NullPointerException("InputStringU");
                return this.inputStringU;
            }
            set
            {
                this.inputStringU = value;
                inputString = value.AsString();
                transformComplete = null;
            }
        }

        /// <summary>
        /// The transformed Unifiable
        /// </summary>
        public Unifiable OutputStringU
        {
            get { return this.TransformU(); }
        }

        #endregion
    }
}
