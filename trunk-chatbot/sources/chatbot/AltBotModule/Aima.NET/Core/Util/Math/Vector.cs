using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Math
{
    using Aima.Core.Util.Datastructure;

    public class Vector : Matrix 
    {
        /// <summary>
        /// Vector is modelled as a matrix with a single column;
        /// </summary>
        /// <param name="size"></param>
        public Vector(int size): base(size, 1) 
        {
        }

        public Vector(IList<double> lst): base(lst.Count, 1) 
        {
            for (int i = 0; i < lst.Count; i++) 
            {
                this.SetValue(i, lst[i]);
            }
        }

        public double GetValue(int i) 
        {
            return this.Get(i, 0);
        }

        public void SetValue(int index, double value) 
        {
            Set(index, 0, value);
        }

        public Vector CopyVector() 
        {
            Vector result = new Vector(GetRowDimension());
            for (int i = 0; i < GetRowDimension(); i++) {
                result.SetValue(i, this.GetValue(i));
            }
            return result;
        }

        public int Size() {
            return GetRowDimension();
        }

        public Vector Minus(Vector v) 
        {
            Vector result = new Vector(this.Size());
            for (int i = 0; i < this.Size(); i++) {
                result.SetValue(i, this.GetValue(i) - v.GetValue(i));
            }
            return result;
        }

        public Vector Plus(Vector v) {
            Vector result = new Vector(this.Size());
            for (int i = 0; i < this.Size(); i++) {
                result.SetValue(i, this.GetValue(i) + v.GetValue(i));
            }
            return result;
        }

        public int IndexHavingMaxValue() {
            if (this.Size() <= 0) {
                throw new ApplicationException("can't perform this op on empty vector");
            }
            int res = 0;
            for (int i = 0; i < this.Size(); i++) {
                if (this.GetValue(i) > this.GetValue(res)) {
                    res = i;
                }
            }
            return res;
        }
    }

}
