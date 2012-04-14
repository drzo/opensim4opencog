using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Util.Math
{
    using System.Runtime.Serialization;

    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// LU Decomposition.
    /// <P>
    /// For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n unit
    /// lower triangular matrix L, an n-by-n upper triangular matrix U, and a
    /// permutation vector piv of length m so that A(piv,:) = L*U. If m < n, then L
    /// is m-by-m and U is m-by-n.
    /// <P>
    /// The LU decompostion with pivoting always exists, even if the matrix is
    /// singular, so the constructor will never fail. The primary use of the LU
    /// decomposition is in the solution of square systems of simultaneous linear
    /// equations. This will fail if isNonsingular() returns false.
    /// </summary>
    // TODO: was serializable, implement if needed
    public class LUDecomposition
    {

        /// <summary>
        /// Array for internal storage of decomposition.
        /// </summary>
        private readonly double[,] lu;

        /// <summary>
        /// Row and column dimensions, and pivot sign.
        /// </summary>
        private readonly int m, n;

        private int pivsign;

        /// <summary>
        /// Internal storage of pivot vector.
        /// </summary>
        private readonly int[] piv;

        /// <summary>
        /// LU Decomposition, a structure to access L, U and piv.
        /// </summary>
        /// <param name="a">Rectangular matrix</param>
        public LUDecomposition(Matrix a) 
        {

            // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

            this.lu = a.GetArrayCopy();
            m = a.GetRowDimension();
            n = a.GetColumnDimension();
            piv = new int[m];
            for (int i = 0; i < m; i++) {
                piv[i] = i;
            }
            pivsign = 1;
            double[] luColj = new double[m];

            // Outer loop.

            for (int j = 0; j < n; j++) {

                // Make a copy of the j-th column to localize references.

                for (int i = 0; i < m; i++) {
                    luColj[i] = this.lu[i,j];
                }

                // Apply previous transformations.

                for (int i = 0; i < m; i++) {
                    // Most of the time is spent in the following dot product.

                    int kmax = System.Math.Min(i, j);
                    double s = 0.0;
                    for (int k = 0; k < kmax; k++) 
                    {
                        s += this.lu[i,k] * luColj[k];
                    }

                    lu[i,j] = luColj[i] -= s;
                }

                // Find pivot and exchange if necessary.

                int p = j;
                for (int i = j + 1; i < m; i++) {
                    if (System.Math.Abs(luColj[i]) > System.Math.Abs(luColj[p])) {
                        p = i;
                    }
                }
                if (p != j) {
                    for (int k = 0; k < n; k++) {
                        double t = this.lu[p,k];
                        this.lu[p,k] = this.lu[j,k];
                        this.lu[j,k] = t;
                    }
                    int k2 = piv[p];
                    piv[p] = piv[j];
                    piv[j] = k2;
                    pivsign = -pivsign;
                }

                // Compute multipliers.

                if (j < m & this.lu[j,j] != 0.0) {
                    for (int i = j + 1; i < m; i++) {
                        this.lu[i,j] /= this.lu[j,j];
                    }
                }
            }
        }

        // TODO: Figure out if below temporary code is needed
        /*
        /// ------------------------ Temporary, experimental code.
        /// ------------------------\
        /// 
        /// \ LU Decomposition, computed by Gaussian elimination. <P> This
        /// constructor computes L and U with the "daxpy"-based elimination algorithm
        /// used in LINPACK and MATLAB. In Java, we suspect the dot-product, Crout
        /// algorithm will be faster. We have temporarily included this constructor
        /// until timing experiments confirm this suspicion. <P> @param A Rectangular
        /// matrix @param linpackflag Use Gaussian elimination. Actual value ignored.
        /// 
        /// @return Structure to access L, U and piv. \
        /// 
        /// public LUDecomposition (Matrix A, int linpackflag) { // Initialize. LU =
        /// A.getArrayCopy(); m = A.getRowDimension(); n = A.getColumnDimension();
        /// piv = new int[m]; for (int i = 0; i < m; i++) { piv[i] = i; } pivsign =
        /// 1; // Main loop. for (int k = 0; k < n; k++) { // Find pivot. int p = k;
        /// for (int i = k+1; i < m; i++) { if (Math.abs(LU[i,k]) >
        /// Math.abs(LU[p,k])) { p = i; } } // Exchange if necessary. if (p != k) {
        /// for (int j = 0; j < n; j++) { double t = LU[p,j]; LU[p,j] = LU[k,j];
        /// LU[k,j] = t; } int t = piv[p]; piv[p] = piv[k]; piv[k] = t; pivsign =
        /// -pivsign; } // Compute multipliers and eliminate k-th column. if
        /// (LU[k,k] != 0.0) { for (int i = k+1; i < m; i++) { LU[i,k] /= LU[k,k];
        /// for (int j = k+1; j < n; j++) { LU[i,j] -= LU[i,k]LU[k,j]; } } } } } \
        /// ------------------------ End of temporary code. ------------------------
         */

        /// <summary>
        /// Is the matrix nonsingular?
        /// </summary>
        /// <returns>true if U, and hence A, is nonsingular.</returns>
        public bool IsNonsingular() 
        {
            for (var j = 0; j < this.n; j++) 
            {
                if (this.lu[j, j] == 0)
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Return lower triangular factor
        /// </summary>
        /// <returns>L</returns>
        public Matrix GetL()
        {
            var x = new Matrix(this.m, this.n);
            var l = x.GetArray();
            for (var i = 0; i < this.m; i++)
            {
                for (var j = 0; j < this.n; j++)
                {
                    if (i > j)
                    {
                        l[i, j] = this.lu[i, j];
                    }
                    else if (i == j)
                    {
                        l[i, j] = 1.0;
                    }
                    else
                    {
                        l[i, j] = 0.0;
                    }
                }
            }
            return x;
        }

        /// <summary>
        /// Return upper triangular factor
        /// </summary>
        /// <returns>U</returns>
        public Matrix GetU()
        {
            var x = new Matrix(this.n, this.n);
            var u = x.GetArray();
            for (var i = 0; i < this.n; i++)
            {
                for (var j = 0; j < this.n; j++)
                {
                    if (i <= j)
                    {
                        u[i, j] = this.lu[i, j];
                    }
                    else
                    {
                        u[i, j] = 0.0;
                    }
                }
            }
            return x;
        }

        /// <summary>
        /// Return pivot permutation vector
        /// </summary>
        /// <returns>piv</returns>
        public int[] GetPivot()
        {
            var p = new int[this.m];
            for (var i = 0; i < this.m; i++)
            {
                p[i] = this.piv[i];
            }
            return p;
        }

        /// <summary>
        /// Return pivot permutation vector as a one-dimensional double array
        /// </summary>
        /// <returns>(double) piv</returns>
        public double[] GetDoublePivot()
        {
            var vals = new double[this.m];
            for (var i = 0; i < this.m; i++)
            {
                vals[i] = this.piv[i];
            }
            return vals;
        }

        /// <summary>
        /// Determinant
        /// </summary>
        /// <returns>det(A)</returns>
        public double Det()
        {
            if (this.m != this.n)
            {
                throw new ArgumentException("Matrix must be square.");
            }
            double d = this.pivsign;
            for (var j = 0; j < this.n; j++)
            {
                d *= this.lu[j, j];
            }
            return d;
        }

        /// <summary>
        /// Solve A*X = B
        /// </summary>
        /// <param name="b">A Matrix with as many rows as A and any number of columns.</param>
        /// <returns>X so that L*U*X = B(piv,:)</returns>
        public Matrix Solve(Matrix b)
        {
            if (b.GetRowDimension() != this.m)
            {
                throw new ArgumentException("Matrix row dimensions must agree.");
            }
            if (!this.IsNonsingular())
            {
                throw new ArgumentException("Matrix is singular.");
            }

            // Copy right hand side with pivoting
            int nx = b.GetColumnDimension();
            Matrix xmat = b.GetMatrix(piv, 0, nx - 1);
            double[,] X = xmat.GetArray();

            // Solve L*Y = B(piv,:)
            for (int k = 0; k < n; k++)
            {
                for (int i = k + 1; i < n; i++)
                {
                    for (int j = 0; j < nx; j++)
                    {
                        X[i, j] -= X[k, j] * this.lu[i, k];
                    }
                }
            }
            // Solve U*X = Y;
            for (int k = n - 1; k >= 0; k--)
            {
                for (int j = 0; j < nx; j++)
                {
                    X[k, j] /= this.lu[k, k];
                }
                for (int i = 0; i < k; i++)
                {
                    for (int j = 0; j < nx; j++)
                    {
                        X[i, j] -= X[k, j] * this.lu[i, k];
                    }
                }
            }
            return xmat;
        }
    }
}
