using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Local
{
    public class Scheduler 
    {

        private readonly int k, limit;

        private readonly double lam;

        public Scheduler(int k, double lam, int limit) 
        {
            this.k = k;
            this.lam = lam;
            this.limit = limit;
        }

        public Scheduler() 
        {
            this.k = 20;
            this.lam = 0.045;
            this.limit = 100;
        }

        public double GetTemp(int t)
        {
            if (t < this.limit)
            {
                var res = this.k * Math.Exp((-1) * this.lam * t);
                return res;
            }

            return 0.0;
        }
    }
}
