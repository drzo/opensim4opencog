using System;
using System.Collections.Generic;

namespace Aima.Core.Util.Datastructure
{
    using System.Collections;
    using System.Globalization;
    using System.IO;
    using System.Linq;
    using System.Runtime.Serialization;
    using System.Text;

    using Aima.Core.Util.Math;

    /// <summary>
    /// Jama = Java Matrix class.
    /// <P>
    /// The Java Matrix Class provides the fundamental operations of numerical linear
    /// algebra. Various constructors create Matrices from two dimensional arrays of
    /// double precision floating point numbers. Various "gets" and "sets" provide
    /// access to submatrices and matrix elements. Several methods implement basic
    /// matrix arithmetic, including matrix addition and multiplication, matrix
    /// norms, and element-by-element array operations. Methods for reading and
    /// printing matrices are also included. All the operations in this version of
    ///the Matrix Class involve real matrices. Complex matrices may be handled in a
    ///future version.
    ///<P>
    /// Five fundamental matrix decompositions, which consist of pairs or triples of
    /// matrices, permutation vectors, and the like, produce results in five
    /// decomposition classes. These decompositions are accessed by the Matrix class
    /// to compute solutions of simultaneous linear equations, determinants, inverses
    /// and other matrix functions. The five decompositions are:
    /// <P>
    /// <UL>
    /// <LI>Cholesky Decomposition of symmetric, positive definite matrices.
    /// <LI>LU Decomposition of rectangular matrices.
    /// <LI>QR Decomposition of rectangular matrices.
    /// <LI>Singular Value Decomposition of rectangular matrices.
    /// <LI>Eigenvalue Decomposition of both symmetric and nonsymmetric square
    /// matrices.
    /// </UL>
    /// <DL>
    /// <DT><B>Example of use:</B></DT>
    /// <P>
    /// <DD>Solve a linear system A x = b and compute the residual norm, ||b - A x||.
    /// <P>
    /// 
    /// <PRE>
    /// double[,] vals = { { 1., 2., 3 }, { 4., 5., 6. }, { 7., 8., 10. } };
    /// Matrix A = new Matrix(vals);
    /// Matrix b = Matrix.random(3, 1);
    /// Matrix x = A.solve(b);
    /// Matrix r = A.times(x).minus(b);
    /// double rnorm = r.normInf();
    /// </PRE>
    /// 
    /// </DD>
    /// </DL>
    /// 
    /// </summary>
    //TODO: this class used to be serializable in java version. Figure out if needed and implement ISerializable in that case
    public class Matrix : ICloneable //, ISerializable 
    {
        /// Array for internal storage of elements.
        private readonly double[,] A;

        /// Row and column dimensions.
        private readonly int m, n;

        /// <summary>
        /// Construct a diagonal Matrix from the given IList of doubles
        /// </summary>
        /// <param name="values"></param>
        /// <returns></returns>
        public static Matrix CreateDiagonalMatrix(IList<double> values) 
        {
            var m = new Matrix(values.Count, values.Count, 0);
            for (int i = 0; i < values.Count; i++) {
                m.Set(i, i, values[i]);
            }
            return m;
        }

        /// <summary>
        /// Construct an m-by-n matrix of zeros.
        /// </summary>
        /// <param name="m">Number of rows.</param>
        /// <param name="n">Number of colums.</param>
        public Matrix(int m, int n) {
            this.m = m;
            this.n = n;
            A = new double[m,n];
        }

        /// <summary>
        /// Construct an m-by-n constant matrix.
        /// </summary>
        /// <param name="m">Number of rows.</param>
        /// <param name="n">Number of colums.</param>
        /// <param name="s">Fill the matrix with this scalar value.</param>
        public Matrix(int m, int n, double s) 
        {
            this.m = m;
            this.n = n;
            A = new double[m,n];
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = s;
                }
            }
        }

        /// <summary>
        /// Construct a matrix from a 2-D array.
        /// </summary>
        /// <param name="a">Two-dimensional array of doubles.</param>
        public Matrix(double[,] a) 
        {
            this.A = a;
        }

        /// <summary>
        /// Construct a matrix quickly without checking arguments.
        /// </summary>
        /// <param name="a">Two-dimensional array of doubles.</param>
        /// <param name="m">Number of rows.</param>
        /// <param name="n">Number of colums.</param>
        public Matrix(double[,] a, int m, int n) 
        {
            this.A = a;
            this.m = m;
            this.n = n;
        }

        /// <summary>
        /// Construct a matrix from a one-dimensional packed array
        /// </summary>
        /// <param name="vals">One-dimensional array of doubles, packed by columns (ala Fortran).</param>
        /// <param name="m">Number of rows.</param>
        public Matrix(double[] vals, int m) 
        {
            this.m = m;
            n = (m != 0 ? vals.Length / m : 0);
            if (m * n != vals.Length) 
            {
                throw new ArgumentException("Array length must be a multiple of m.");
            }
            A = new double[m,n];
            for (int i = 0; i < m; i++) 
            {
                for (int j = 0; j < n; j++) 
                {
                    A[i,j] = vals[i + j * m];
                }
            }
        }

        /// <summary>
        /// Construct a matrix from a copy of a 2-D array.
        /// </summary>
        /// <param name="a">Two-dimensional array of doubles.</param>
        /// <returns></returns>
        public static Matrix ConstructWithCopy(double[,] a) 
        {
            var m = a.GetLength(0);
            var n = a.GetLength(1);
            var x = new Matrix(m, n);
            var c = x.GetArray();
            for (var i = 0; i < m; i++) 
            {
                for (var j = 0; j < n; j++) 
                {
                    c[i,j] = a[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Make a deep copy of a matrix
        /// </summary>
        /// <returns></returns>
        public Matrix Copy() 
        {
            var x = new Matrix(m, n);
            var c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Clone the Matrix object.
        /// </summary>
        /// <returns></returns>
        public object Clone() 
        {
            return this.Copy();
        }

        /// <summary>
        /// Access the internal two-dimensional array.
        /// </summary>
        /// <returns>Pointer to the two-dimensional array of matrix elements.</returns>
        public double[,] GetArray() 
        {
            return A;
        }

        /// <summary>
        /// Copy the internal two-dimensional array.
        /// </summary>
        /// <returns>Two-dimensional array copy of matrix elements.</returns>
        public double[,] GetArrayCopy() 
        {
            var C = new double[m,n];
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    C[i,j] = A[i,j];
                }
            }
            return C;
        }

        /// <summary>
        /// Make a one-dimensional column packed copy of the internal array.
        /// </summary>
        /// <returns>Matrix elements packed in a one-dimensional array by columns.</returns>
        public double[] GetColumnPackedCopy() 
        {
            var vals = new double[m * n];
            for (int i = 0; i < m; i++) 
            {
                for (int j = 0; j < n; j++) 
                {
                    vals[i + j * m] = A[i,j];
                }
            }
            return vals;
        }

        /// <summary>
        /// Make a one-dimensional row packed copy of the internal array.
        /// </summary>
        /// <returns>Matrix elements packed in a one-dimensional array by rows.</returns>
        public double[] GetRowPackedCopy() 
        {
            var vals = new double[m * n];
            for (int i = 0; i < m; i++) 
            {
                for (int j = 0; j < n; j++) 
                {
                    vals[i * n + j] = A[i,j];
                }
            }
            return vals;
        }

        /// <summary>
        /// Get row dimension.
        /// </summary>
        /// <returns> m,  the number of rows.</returns>
        public int GetRowDimension() 
        {
            return m;
        }

        /// <summary>
        /// Get column dimension.
        /// </summary>
        /// <returns>n, the number of columns.</returns>
        public int GetColumnDimension() 
        {
            return n;
        }

        /// <summary>
        /// Get a single element.
        /// </summary>
        /// <param name="i">Row index.</param>
        /// <param name="j">Column index.</param>
        /// <returns>A(i,j)</returns>
        public double Get(int i, int j) 
        {
            return A[i,j];
        }

        /// <summary>
        /// Get a submatrix.
        /// </summary>
        /// <param name="i0">Initial row index</param>
        /// <param name="i1">Final row index</param>
        /// <param name="j0">Initial column index</param>
        /// <param name="j1">Final column index</param>
        /// <returns>A(i0:i1,j0:j1)</returns>
        public Matrix GetMatrix(int i0, int i1, int j0, int j1) 
        {
            var x = new Matrix(i1 - i0 + 1, j1 - j0 + 1);
            var b = x.GetArray();
            for (int i = i0; i <= i1; i++) {
                for (int j = j0; j <= j1; j++) {
                    b[i - i0,j - j0] = A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Get a submatrix.
        /// </summary>
        /// <param name="r">Array of row indices.</param>
        /// <param name="c">Array of column indices.</param>
        /// <returns>A(r(:),c(:))</returns>
        public Matrix GetMatrix(int[] r, int[] c) 
        {
            var X = new Matrix(r.Length, c.Length);
            double[,] B = X.GetArray();
            for (int i = 0; i < r.Length; i++) {
                for (int j = 0; j < c.Length; j++) {
                    B[i,j] = A[r[i],c[j]];
                }
            }
            return X;
        }

        /// <summary>
        /// Get a submatrix.
        /// </summary>
        /// <param name="i0">Initial row index</param>
        /// <param name="i1">Final row index</param>
        /// <param name="c">Array of column indices.</param>
        /// <returns>A(i0:i1,c(:))</returns>
        public Matrix GetMatrix(int i0, int i1, int[] c) 
        {
            var x = new Matrix(i1 - i0 + 1, c.Length);
            double[,] b = x.GetArray();
            for (int i = i0; i <= i1; i++) {
                for (int j = 0; j < c.Length; j++) {
                    b[i - i0,j] = A[i,c[j]];
                }
            }
            return x;
        }

        /// <summary>
        /// Get a submatrix.
        /// </summary>
        /// <param name="r">Array of row indices.</param>
        /// <param name="j0">Initial column index</param>
        /// <param name="j1">Final column index</param>
        /// <returns>A(r(:),j0:j1)</returns>
        public Matrix GetMatrix(int[] r, int j0, int j1) {
            var x = new Matrix(r.Length, j1 - j0 + 1);
            double[,] b = x.GetArray();
            for (int i = 0; i < r.Length; i++) {
                for (int j = j0; j <= j1; j++) {
                    b[i,j - j0] = A[r[i],j];
                }
            }
            return x;
        }

        /// <summary>
        /// Set a single element.
        /// </summary>
        /// <param name="i">Row index.</param>
        /// <param name="j">Column index.</param>
        /// <param name="s">A(i,j).</param>
        public void Set(int i, int j, double s) {
            A[i,j] = s;
        }

        /// <summary>
        /// Set a submatrix.
        /// </summary>
        /// <param name="i0">Initial row index</param>
        /// <param name="i1">Final row index</param>
        /// <param name="j0">Initial column index</param>
        /// <param name="j1">Final column index</param>
        /// <param name="x">A(i0:i1,j0:j1)</param>
        public void SetMatrix(int i0, int i1, int j0, int j1, Matrix x) 
        {
            for (int i = i0; i <= i1; i++) {
                for (int j = j0; j <= j1; j++) {
                    A[i,j] = x.Get(i - i0, j - j0);
                }
            }
        }

        /// <summary>
        /// Set a submatrix.
        /// </summary>
        /// <param name="r">Array of row indices.</param>
        /// <param name="c">Array of column indices.</param>
        /// <param name="x">A(r(:),c(:))</param>
        public void SetMatrix(int[] r, int[] c, Matrix x) 
        {
            for (int i = 0; i < r.Length; i++) {
                for (int j = 0; j < c.Length; j++) {
                    A[r[i],c[j]] = x.Get(i, j);
                }
            }
        }

        /// <summary>
        /// Set a submatrix.
        /// </summary>
        /// <param name="r">Array of row indices.</param>
        /// <param name="j0">Initial column index</param>
        /// <param name="j1">Final column index</param>
        /// <param name="x">A(r(:),j0:j1)</param>
        public void SetMatrix(int[] r, int j0, int j1, Matrix x) 
        {
            for (int i = 0; i < r.Length; i++) {
                for (int j = j0; j <= j1; j++) {
                    A[r[i],j] = x.Get(i, j - j0);
                }
            }
        }

        /// <summary>
        /// Set a submatrix.
        /// 
        /// @param i0
        ///            
        /// @param i1
        ///            
        /// @param c
        ///            
        /// @param X
        ///            
        /// @exception ArrayIndexOutOfBoundsException
        ///                Submatrix indices
        /// </summary>
        /// <param name="i0">Initial row index</param>
        /// <param name="i1">Final row index</param>
        /// <param name="c">Array of column indices.</param>
        /// <param name="x">A(i0:i1,c(:))</param>
        public void SetMatrix(int i0, int i1, int[] c, Matrix x) 
        {
            for (int i = i0; i <= i1; i++) {
                for (int j = 0; j < c.Length; j++) {
                    A[i,c[j]] = x.Get(i - i0, j);
                }
            }
        }

        /// <summary>
        /// Matrix transpose.
        /// </summary>
        /// <returns></returns>
        public Matrix Transpose() 
        {
            var x = new Matrix(n, m);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[j,i] = A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// One norm
        /// </summary>
        /// <returns>maximum column sum.</returns>
        public double Norm1() 
        {
            double f = 0;
            for (int j = 0; j < n; j++) {
                double s = 0;
                for (int i = 0; i < m; i++) {
                    s += System.Math.Abs(A[i,j]);
                }
                f = System.Math.Max(f, s);
            }
            return f;
        }

        //TODO: figure out if below is needed
        /**
        /// Two norm
        /// 
        /// @return maximum singular value.
         */

        // public double norm2 () {
        // return (new SingularValueDecomposition(this).norm2());
        // }

        
        /// <summary>
        /// Infinity norm
        /// </summary>
        /// <returns>maximum row sum.</returns>
        public double NormInf() 
        {
            double f = 0;
            for (int i = 0; i < m; i++) 
            {
                double s = 0;
                for (int j = 0; j < n; j++) 
                {
                    s += System.Math.Abs(A[i,j]);
                }
                f = System.Math.Max(f, s);
            }
            return f;
        }

        //TODO: figure out if below is needed
        /**
        /// Frobenius norm
        /// 
        /// @return sqrt of sum of squares of all elements.
         */

        // public double normF () {
        // double f = 0;
        // for (int i = 0; i < m; i++) {
        // for (int j = 0; j < n; j++) {
        // f = Maths.hypot(f,A[i,j]);
        // }
        // }
        // return f;
        // }

        /// <summary>
        /// Unary minus
        /// </summary>
        /// <returns>-A</returns>
        public Matrix Uminus() 
        {
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = -A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// C = A + B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A + B</returns>
        public Matrix Plus(Matrix b) 
        {
            this.CheckMatrixDimensions(b);
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = A[i,j] + b.A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// A = A + B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A + B</returns>
        public Matrix PlusEquals(Matrix b) 
        {
            this.CheckMatrixDimensions(b);
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = A[i,j] + b.A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// C = A - B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A - B</returns>
        public Matrix Minus(Matrix b) {
            this.CheckMatrixDimensions(b);
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = A[i,j] - b.A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// A = A - B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A - B</returns>
        public Matrix MinusEquals(Matrix b) {
            this.CheckMatrixDimensions(b);
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = A[i,j] - b.A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// Element-by-element multiplication, C = A.*B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A.*B</returns>
        public Matrix ArrayTimes(Matrix b) 
        {
            this.CheckMatrixDimensions(b);
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = A[i,j] * b.A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Element-by-element multiplication in place, A = A.*B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A.*B</returns>
        public Matrix ArrayTimesEquals(Matrix b) {
            this.CheckMatrixDimensions(b);
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = A[i,j] * b.A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// Element-by-element right division, C = A./B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A./B</returns>
        public Matrix ArrayRightDivide(Matrix b) {
            this.CheckMatrixDimensions(b);
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = A[i,j] / b.A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Element-by-element right division in place, A = A./B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A./B</returns>
        public Matrix ArrayRightDivideEquals(Matrix b) {
            this.CheckMatrixDimensions(b);
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = A[i,j] / b.A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// Element-by-element left division, C = A.\B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A.\B</returns>
        public Matrix ArrayLeftDivide(Matrix b) {
            this.CheckMatrixDimensions(b);
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = b.A[i,j] / A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Element-by-element left division in place, A = A.\B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>A.\B</returns>
        public Matrix ArrayLeftDivideEquals(Matrix b) {
            this.CheckMatrixDimensions(b);
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = b.A[i,j] / A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// Multiply a matrix by a scalar, C = s*A
        /// </summary>
        /// <param name="s">scalar</param>
        /// <returns>s*A</returns>
        public Matrix Times(double s) {
            var x = new Matrix(m, n);
            double[,] c = x.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i,j] = s * A[i,j];
                }
            }
            return x;
        }

        /// <summary>
        /// Multiply a matrix by a scalar in place, A = s*A
        /// </summary>
        /// <param name="s">scalar</param>
        /// <returns>replace A by s*A</returns>
        public Matrix TimesEquals(double s) {
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    A[i,j] = s * A[i,j];
                }
            }
            return this;
        }

        /// <summary>
        /// Linear algebraic matrix multiplication, A/// B
        /// </summary>
        /// <param name="b">another matrix</param>
        /// <returns>Matrix product, A * B</returns>
        public Matrix Times(Matrix b) {
            if (b.m != n) {
                throw new ArgumentException("Matrix inner dimensions must agree.");
            }
            var X = new Matrix(m, b.n);
            double[,] c = X.GetArray();
            var bcolj = new double[n];
            for (int j = 0; j < b.n; j++) {
                for (int k = 0; k < n; k++) {
                    bcolj[k] = b.A[k,j];
                }
                for (int i = 0; i < m; i++) {
                    double s = 0;
                    for (int k = 0; k < n; k++) {
                        s += A[i,k] * bcolj[k];
                    }
                    c[i,j] = s;
                }
            }
            return X;
        }

        /// <summary>
        /// LU Decomposition
        /// </summary>
        /// <returns>LUDecomposition</returns>
        public LUDecomposition lu() {
            return new LUDecomposition(this);
        }

        //TODO: figure out if this commented out code is needed
        // /** QR Decomposition
        // @return QRDecomposition
        // @see QRDecomposition
        // */
        //
        // public QRDecomposition qr () {
        // return new QRDecomposition(this);
        // }
        //
        // /** Cholesky Decomposition
        // @return CholeskyDecomposition
        // @see CholeskyDecomposition
        // */
        //
        // public CholeskyDecomposition chol () {
        // return new CholeskyDecomposition(this);
        // }
        //
        // /** Singular Value Decomposition
        // @return SingularValueDecomposition
        // @see SingularValueDecomposition
        // */
        //
        // public SingularValueDecomposition svd () {
        // return new SingularValueDecomposition(this);
        // }
        //
        // /** Eigenvalue Decomposition
        // @return EigenvalueDecomposition
        // @see EigenvalueDecomposition
        // */
        //
        // public EigenvalueDecomposition eig () {
        // return new EigenvalueDecomposition(this);
        // }

        /// <summary>
        /// Solve A*X = B
        /// </summary>
        /// <param name="b">right hand side</param>
        /// <returns>solution if A is square, least squares solution otherwise</returns>
        public Matrix Solve(Matrix b) 
        {
            // assumed m == n
            return new LUDecomposition(this).Solve(b);

        }

        /// <summary>
        /// Solve X*A = B, which is also A'*X' = B'
        /// </summary>
        /// <param name="b">right hand side</param>
        /// <returns>solution if A is square, least squares solution otherwise.</returns>
        public Matrix SolveTranspose(Matrix b) 
        {
            return this.Transpose().Solve(b.Transpose());
        }

        /// <summary>
        /// Matrix inverse or pseudoinverse
        /// </summary>
        /// <returns>inverse(A) if A is square, pseudoinverse otherwise.</returns>
        public Matrix Inverse() 
        {
            return this.Solve(Identity(m, m));
        }

        /// <summary>
        /// Matrix determinant
        /// </summary>
        /// <returns>determinant</returns>
        public double Det() 
        {
            return new LUDecomposition(this).Det();
        }

        //TODO: figure out if this commented out code is needed
        /**
        /// Matrix rank
        /// 
        /// @return effective numerical rank, obtained from SVD.
         */

        // public int rank () {
        // return new SingularValueDecomposition(this).rank();
        // }
        //
        // /** Matrix condition (2 norm)
        // @return ratio of largest to smallest singular value.
        // */
        //
        // public double cond () {
        // return new SingularValueDecomposition(this).cond();
        // }

        /// <summary>
        /// Matrix trace.
        /// </summary>
        /// <returns>sum of the diagonal elements.</returns>
        public double Trace() 
        {
            double t = 0;
            for (int i = 0; i < System.Math.Min(m, n); i++) {
                t += A[i,i];
            }
            return t;
        }

        /// <summary>
        /// Generate matrix with random elements
        /// </summary>
        /// <param name="m">Number of rows.</param>
        /// <param name="n">Number of colums.</param>
        /// <returns>An m-by-n matrix with uniformly distributed random elements.</returns>
        public static Matrix Random(int m, int n) 
        {
            var a = new Matrix(m, n);
            var x = a.GetArray();
            for (var i = 0; i < m; i++) {
                for (var j = 0; j < n; j++) {
                    x[i,j] = new Random().NextDouble(); // TODO: Make sure that Random().NextDouble() works the same as math.rand() in java
                }
            }
            return a;
        }

        /// <summary>
        /// Generate identity matrix
        /// </summary>
        /// <param name="m">Number of rows.</param>
        /// <param name="n">Number of colums.</param>
        /// <returns>An m-by-n matrix with ones on the diagonal and zeros elsewhere.</returns>
        public static Matrix Identity(int m, int n) 
        {
            var a = new Matrix(m, n);
            double[,] x = a.GetArray();
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    x[i,j] = (i == j ? 1.0 : 0.0);
                }
            }
            return a;
        }

        /// <summary>
        /// Print the matrix to stdout. Line the elements up in columns with a
        /// Fortran-like 'Fw.d' style format.
        /// </summary>
        /// <param name="w">Column width.</param>
        /// <param name="d">Number of digits after the decimal.</param>
        public void Print(int w, int d) {
            this.Print(new StreamWriter(Console.OpenStandardOutput()), w, d);
        }

        /// <summary>
        /// Print the matrix to the output stream. Line the elements up in columns
        /// with a Fortran-like 'Fw.d' style format.
        /// </summary>
        /// <param name="output"> Output stream.</param>
        /// <param name="w">Column width.</param>
        /// <param name="d">Number of digits after the decimal.</param>
        public void Print(StreamWriter output, int w, int d)
        {

            
            NumberFormatInfo format = CultureInfo.GetCultureInfo("en-US").NumberFormat;
            format.NumberDecimalDigits = d;
            //TODO: make sure that below line really removes number groupings
            format.NumberGroupSizes = new[] { 0 };
            this.Print(output, format, w + 2);

        }

        /// <summary>
        /// Print the matrix to stdout. Line the elements up in columns. Use the
        /// format object, and right justify within columns of width characters. Note
        /// that is the matrix is to be read back in, you probably will want to use a
        /// NumberFormatInfo that is set to US Locale.
        /// </summary>
        /// <param name="format">A Formatting object for individual elements.</param>
        /// <param name="width">Field width for each column.</param>
        public void Print(NumberFormatInfo format, int width) 
        {
            this.Print(new StreamWriter(Console.OpenStandardOutput()), format, width);
        }

        //TODO: evaluate if this comment is valid for C#
        // DecimalFormat is a little disappointing coming from Fortran or C's
        // printf.
        // Since it doesn't pad on the left, the elements will come out different
        // widths. Consequently, we'll pass the desired column width in as an
        // argument and do the extra padding ourselves.

        /// <summary>
        /// Print the matrix to the output stream. Line the elements up in columns.
        /// Use the format object, and right justify within columns of width
        /// characters. Note that is the matrix is to be read back in, you probably
        /// will want to use a NumberFormat that is set to US Locale.
        /// </summary>
        /// <param name="output">the output stream.</param>
        /// <param name="format">A formatting object to format the matrix elements</param>
        /// <param name="width">Column width.</param>
        public void Print(StreamWriter output, NumberFormatInfo format, int width) 
        {
            output.WriteLine(); // start on new line.
            for (int i = 0; i < m; i++) 
            {
                for (int j = 0; j < n; j++)
                {
                    String s = A[i, j].ToString(format); // format the number
                    
                    int padding = System.Math.Max(1, width - s.Length); // At _least_ 1
                    // space
                    for (int k = 0; k < padding; k++)
                        output.Write(' ');
                    output.Write(s);
                }
                output.WriteLine();
            }
            output.WriteLine(); // end with blank line.
        }

        /// <summary>
        /// Read a matrix from a stream. The format is the same the print method, so
        /// printed matrices can be read back in (provided they were printed using US
        /// Locale). Elements are separated by whitespace, all the elements for each
        /// row appear on a single line, the last row is followed by a blank line.
        /// </summary>
        /// <param name="input">the input stream.</param>
        /// <returns></returns>
        public static Matrix Read(StreamReader input)
        {
            var v = new List<List<double>>();
            string line = "";
           
            // Ignore initial empty lines
            while (line == "" && !input.EndOfStream)
            {
                line = input.ReadLine();
            }
           
            if (input.EndOfStream || line == null)
                throw new IOException("Unexpected EOF on matrix read.");

            var row = new List<double>(line.Split(" ".ToCharArray()).Cast<double>());
            v.Add(row); // Read & store 1st

            int n = row.Count; // Now we've got the number of columns!

            line = input.ReadLine();
            while (!input.EndOfStream && line != null) 
            {
                row = new List<double>(line.Split(" ".ToCharArray()).Cast<double>());

                if (row.Count > n)
                {
                    throw new IOException("Row " + (v.Count + 1) + " is too long.");
                }
                else if (row.Count < n)
                {
                    throw new IOException("Row " + (v.Count + 1) + " is too short.");
                }

                v.Add(row);    
                line = input.ReadLine();
            }
            int m = v.Count; // Now we've got the number of rows.
            
            var a = new double[m,n];
            
            for (int i=0;i<m;i++)
            {
                for (int j=0;j<n; j++)
                {
                    a[i, j] = v[i][j];
                }
            }

            return new Matrix(a);
        }

        public override string ToString() {
            var buf = new StringBuilder();
            for (var i = 0; i < this.GetRowDimension(); i++) 
            {

                for (var j = 0; j < this.GetColumnDimension(); j++) 
                {
                    buf.Append(this.Get(i, j));
                    buf.Append(" ");
                }
                buf.Append("\n");
            }

            return buf.ToString();
        }


        /// <summary>
        /// Check if size(A) == size(B)
        /// </summary>
        /// <param name="b"></param>
        private void CheckMatrixDimensions(Matrix b) 
        {
            if (b.m != this.m || b.n != this.n) 
            {
                throw new ArgumentException("Matrix dimensions must agree.");
            }
        }
    }

}
