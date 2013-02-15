using System;
using MushDLR223.Utilities;

namespace AltAIMLbot.Utils
{
    public class ResultCache //:TextPatternUtils
    {
        private Unifiable _value;
        private bool _valid;
        public bool IsValid
        {
            get
            {
                if (!_valid) return false;
                if (!Unifiable.IsNullOrEmpty(_value)) return true;
                return true;
            }
            set
            {

                if (value == _valid) return;
                if (value)
                {
                    if (Unifiable.IsNull(_value))
                    {
                        DLRConsole.DebugWriteLine("Set _RecurseResult to String.Empty at least!");
                    }
                }
                _valid = value;
            }
        }

        public override string ToString()
        {
            if (IsValid) return _value;
            return "!Valid " + Unifiable.DescribeUnifiable(_value);
        }

        public Unifiable Value
        {
            get
            {
                if (!_valid) return Unifiable.NULL;
                if (!Unifiable.IsNullOrEmpty(_value)) return _value;
                return _value;
            }
            set
            {
                _value = value;
                IsValid = true;
            }
        }

        public void Reset()
        {
            _valid = false;
            _value = null;
        }
    }
}