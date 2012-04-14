using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Math
{
    /// <summary>
    /// see: http://demonstrations.wolfram.com/MixedRadixNumberRepresentations/ for useful example.
    /// </summary>
    public class MixedRadixNumber
        //TODO: Used to inherit from Number is that required?
    {
        private long value = 0L;
        private long maxValue = 0L;
        private int[] radixs = null;
        private int[] currentNumeralValue = null;
        private bool recalculate = true;

        public MixedRadixNumber(long value, int[] radixs) 
        {
            this.value = value;
            this.radixs = new int[radixs.Length];
            radixs.CopyTo(this.radixs,0);
            this.CalculateMaxValue();
        }

        public MixedRadixNumber(long value, IList<int> radixs) 
        {
            this.value = value;
            this.radixs = new int[radixs.Count];
            radixs.CopyTo(this.radixs, 0);
            this.CalculateMaxValue();
        }

        public long GetMaxAllowedValue() 
        {
            return maxValue;
        }

        public bool Increment() 
        {
            if (value < maxValue) 
            {
                value++;
                recalculate = true;
                return true;
            }

            return false;
        }

        public bool Decrement() 
        {
            if (value > 0) 
            {
                value--;
                recalculate = true;
                return true;
            }
            return false;
        }

        public int GetCurrentNumeralValue(int atPosition) 
        {
            if (atPosition >= 0 && atPosition < radixs.Length) 
            {
                if (recalculate) 
                {
                    long quotient = value;
                    for (int i = 0; i < radixs.Length; i++) 
                    {
                        if (0 != quotient) 
                        {
                            currentNumeralValue[i] = (int) quotient % radixs[i];
                            quotient = quotient / radixs[i];
                        } 
                        else 
                        {
                            currentNumeralValue[i] = 0;
                        }
                    }
                    recalculate = false;
                }
                return currentNumeralValue[atPosition];
            }
            throw new ArgumentOutOfRangeException("Argument atPosition must be >=0 and < " + radixs.Length);
        }

        public int IntValue() 
        {
            return (int) this.LongValue();
        }

        public long LongValue() 
        {
            return value;
        }

        public float FloatValue() 
        {
            return this.LongValue();
        }

        public double DoubleValue() 
        {
            return this.LongValue();
        }

        public override string ToString() 
        {
            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < this.radixs.Length; i++) 
            {
                sb.Append("[");
                sb.Append(this.GetCurrentNumeralValue(i));
                sb.Append("]");
            }

            return sb.ToString();
        }

        private void CalculateMaxValue()
        {
            if (0 == radixs.Length)
            {
                throw new ArgumentException("At least 1 radix must be defined.");
            }
            for (int i = 0; i < radixs.Length; i++)
            {
                if (radixs[i] < 2)
                {
                    throw new ArgumentException("Invalid radix, must be >= 2");
                }
            }

            // Calcualte the maxValue allowed
            this.maxValue = this.radixs[0];
            for (int i = 1; i < this.radixs.Length; i++)
            {
                this.maxValue *= this.radixs[i];
            }
            this.maxValue -= 1;

            if (this.value > this.maxValue)
            {
                throw new ArgumentOutOfRangeException(
                    "The value [" + this.value + "] cannot be represented with the radixs provided, max value is " + this.maxValue);
            }

            this.currentNumeralValue = new int[this.radixs.Length];
        }
    }

}
