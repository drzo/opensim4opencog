//
//  THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
//  KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
//  PURPOSE. IT CAN BE DISTRIBUTED FREE OF CHARGE AS LONG AS THIS HEADER 
//  REMAINS UNCHANGED.
//
//  Email:  gustavo_franco@hotmail.com
//
//  Copyright (C) 2006 Franco, Gustavo 
//
using System;
using System.Data;
using System.Text;
using System.Drawing;
using System.Windows.Forms;
using System.ComponentModel;
using System.Collections.Generic;

using cogbot.TheOpenSims.Navigation;

namespace cogbot.TheOpenSims.Navigation.Debug
{

    #region Enums
    [Author("Franco, Gustavo")]
    public enum DrawModeSetup
    {
        None    = 0,
        Start   = 1,
        End     = 2,
        Block   = 3
    }
    #endregion

    [Author("Franco, Gustavo")]
    public class PanelPathFinder : UserControl
    {
        #region Variables Declaration
        private byte                mNodeWeight     = 1;
        private int mGridSize = 1;
        public int GridX = 1;
        public int GridY = 1;
        public SimPathStore PathStore;// byte[,] mMatrix = null;
        private Point               mStart          = Point.Empty;
        private Point               mEnd            = Point.Empty;
        private DrawModeSetup       mDrawMode       = DrawModeSetup.None;
        private HeuristicFormula    mFormula        = HeuristicFormula.Manhattan;
        #endregion

        #region Constructors
        public PanelPathFinder(SimPathStore simPathStore)
        {
            InitializeComponent();
            PathStore = simPathStore;
           // ResetMatrix(mMatrix);
        }
        #endregion

        #region Properties
        public byte[,] Matrix
        {
            get { return PathStore.mMatrix; }
        }

        public int GridSize
        {
            get { return mGridSize; }
            set
            {
                mGridSize = value;
                CenterOnStart();
                Invalidate();

            }
        }

        public DrawModeSetup DrawModeSetup
        {
            get { return mDrawMode; }
            set { mDrawMode = value; }
        }

        public byte NodeWeight
        {
            get { return mNodeWeight; }
            set { mNodeWeight = value; }
        }

        public Point Start
        {
            get { return mStart; }
            set { mStart = value; }
        }

        public Point End
        {
            get { return mEnd; }
            set { mEnd = value; }
        }

        public HeuristicFormula Formula
        {
            get { return mFormula; }
            set { mFormula = value; }
        }
        #endregion

        #region Methods
        public void ResetMatrix()
        {
            PathStore.Clear();
            mStart    = Point.Empty;
            mEnd      = Point.Empty;
        }

        public void DrawDebug0(int parentX, int parentY, int x, int y, PathFinderNodeType type, int totalCost, int cost)
        {
            Color c = Color.Empty;
            switch(type)
            {
                case PathFinderNodeType.Close:
                    c = Color.DarkSlateBlue;
                    break;
                case PathFinderNodeType.Current: 
                    c = Color.Yellow;
                    break;
                case PathFinderNodeType.End:
                  //  if (true) return;
                    c = Color.Red;
                    break;
                case PathFinderNodeType.Open:
                    c = Color.Green;
                    break;
                case PathFinderNodeType.Path:
                    c = Color.Blue;
                    break;
                case PathFinderNodeType.Start:
                    c = Color.Green;
                    break;
            }
            try
            {
                Graphics g = Graphics.FromHwnd(this.Handle);

                Rectangle internalRec = new Rectangle((x * mGridSize) + 2, (TRANSPOSE(y) * mGridSize) + 2, mGridSize - 4, mGridSize - 4);

                if (type == PathFinderNodeType.Open)
                    using (Brush brush = new SolidBrush(Color.FromArgb(255,240,240,240)))
                        g.FillRectangle(brush, internalRec);

                using (Pen pen = new Pen(c))
                    g.DrawRectangle(pen, internalRec);

              //  if (type == PathFinderNodeType.Open)
                //    g.DrawLine(Pens.Brown, (parentX * mGridSize) + mGridSize / 2, (TRANSPOSE(parentY) * mGridSize) + mGridSize / 2, (x * mGridSize) + mGridSize / 2, (TRANSPOSE(y) * mGridSize) + mGridSize / 2);

                if (type == PathFinderNodeType.Path)
                    using (Brush brush = new SolidBrush(c))
                        g.FillRectangle(brush, internalRec);

                if (totalCost != -1)
                {
                    internalRec.Inflate(new Size(1,1));
                    internalRec.Height /= 2;
                    g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.ClearTypeGridFit;
                    using (Font f = new System.Drawing.Font("Verdana", 0.29F * mGridSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0))))
                        g.DrawString(totalCost.ToString(), f, Brushes.Black, (RectangleF) internalRec);
                    internalRec.Y += internalRec.Height;
                    using (Font f = new System.Drawing.Font("Verdana", 0.29F * mGridSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0))))
                        g.DrawString(cost.ToString(), f, Brushes.Black, (RectangleF) internalRec);
                }

                g.Dispose();
            }
            catch(Exception){}
        }
        #endregion


        private void CenterOnStart()
        {
            ///throw new Exception("The method or operation is not implemented.");
            OffsetXY((int)(GridX * GridSize * PathStore.POINTS_PER_METER),(int)( GridY * GridSize * PathStore.POINTS_PER_METER));
        }

        private void OffsetXY(int X, int Y)
        {
            if (this.Parent != null)
            {
                Size PS = this.Parent.Size;
                int VisualX = PS.Width - 163;
                int VisualY = PS.Height - 55;
                this.Location = new System.Drawing.Point(-X, -Y);//- X, -1 - Y);
                this.Size = new System.Drawing.Size(VisualX - Location.X, VisualY - Location.Y);
            }
        }
        
        #region Overrides
        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics g = e.Graphics;
            if (PathStore != null)
            {
                for (int y = (e.ClipRectangle.Y / mGridSize) * mGridSize; y <= e.ClipRectangle.Bottom; y += mGridSize)
                    for (int x = (e.ClipRectangle.X / mGridSize) * mGridSize; x <= e.ClipRectangle.Right; x += mGridSize)
                    {
                        int sx = x / mGridSize;
                        int sy = y / mGridSize;

                        // Lets render the obstacules
                        Color color = Color.Empty;
                        if (sx < PathStore.MAPSPACE && sy < PathStore.MAPSPACE)
                            using (SolidBrush brush = ColourForByte(sx, TRANSPOSE(sy)))
                            g.FillRectangle(brush, x, y, mGridSize, mGridSize);

                        ////Lets render start and end
                        //if (sx == mStart.X && sy == mStart.Y)
                        //    using (SolidBrush brush = new SolidBrush(Color.Green))
                        //        g.FillRectangle(brush, x, y, mGridSize, mGridSize);

                        //if (sx == mEnd.X && sy == mEnd.Y)
                        //    using (SolidBrush brush = new SolidBrush(Color.Red))
                        //        g.FillRectangle(brush, x, y, mGridSize, mGridSize);
                    }
            }

            Color c = Color.Black;
            if (mGridSize >= 20)
            {
                using (Pen pen = new Pen(c))
                {
                    for (int y = (e.ClipRectangle.Y / mGridSize) * mGridSize; y <= e.ClipRectangle.Bottom; y += mGridSize)
                        g.DrawLine(pen, e.ClipRectangle.X, y, e.ClipRectangle.Right, y);

                    for (int x = (e.ClipRectangle.X / mGridSize) * mGridSize; x <= e.ClipRectangle.Right; x += mGridSize)
                        g.DrawLine(pen, x, e.ClipRectangle.Y, x, e.ClipRectangle.Bottom);
                }
            }
            base.OnPaint(e);
        }

        private int TRANSPOSE(int sy)
        {
           return PathStore.MAPSPACE-1-sy;
        }


       // SolidBrush lastsb;
       // byte lastColorByte;
        private SolidBrush ColourForByte(int x,int y)
        {
            Color sb = PathStore.GetColor(x,y);
            SolidBrush lastsb = new SolidBrush(sb);
           // lastColorByte= p;
            return lastsb;
        }

        ToolTip tip = new ToolTip();
        protected override void OnMouseMove(MouseEventArgs e)
        {
            int x = e.X / mGridSize;
            int sy = e.Y / mGridSize;
            int y = TRANSPOSE(sy);
            if (x >= PathStore.MAPSPACE || x < 0 || y < 0 || y >= PathStore.MAPSPACE) return;

            if (e.Button == MouseButtons.None || mDrawMode == DrawModeSetup.None)
            {
                SimWaypoint o = PathStore.mWaypoints[x, y];
                if (o != null)
                {
                    tip.SetToolTip(this, o.OccupiedString() + " " + PathStore.mMatrix[x, y]);
                }
                else
                {             
                    tip.SetToolTip(this, "" + PathStore.mMatrix[x, y]);
                }
                return;
            }

            byte[,] mMatrix = PathStore.mMatrix;
            switch (mDrawMode)
            {
                case DrawModeSetup.Start:
                    this.Invalidate(new Rectangle(mStart.X * mGridSize, mStart.Y * mGridSize, mGridSize, mGridSize));
                    mStart          = new Point(x,y);
                    PathStore.SetPassable(x / PathStore.POINTS_PER_METER, y / PathStore.POINTS_PER_METER);// mMatrix[x, y] = 1;
                    break;
                case DrawModeSetup.End:
                    this.Invalidate(new Rectangle(mEnd.X * mGridSize, mEnd.Y * mGridSize, mGridSize, mGridSize));
                    mEnd            = new Point(x,y);
                    PathStore.SetPassable(x / PathStore.POINTS_PER_METER, y / PathStore.POINTS_PER_METER);// mMatrix[x, y] = 1;
                    break;
                case DrawModeSetup.Block:
                    if (e.Button == (MouseButtons.Left | MouseButtons.Right))
                        SetMatrix(x, y, ((byte)(mMatrix[x, y] - mNodeWeight > 1 ? mMatrix[x, y] - mNodeWeight : 1)));
                    else if (e.Button == MouseButtons.Left)
                        SetMatrix(x, y, mNodeWeight);
                    else if (e.Button == MouseButtons.Right)
                        SetMatrix(x,y,(byte) (mMatrix[x,y] + mNodeWeight < 256 ? mMatrix[x,y] + mNodeWeight : 255));
                    break;
            }

            this.Invalidate(new Rectangle(x * mGridSize, sy * mGridSize, mGridSize, mGridSize));
            base.OnMouseMove(e);
        }


        protected override void OnMouseDown(MouseEventArgs e)
        {
            this.OnMouseMove(e);
            base.OnMouseDown(e);
        }
        #endregion
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // PanelPathFinder
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.White;
            this.Name = "PanelPathFinder";
            this.Size = new System.Drawing.Size(642, 516);
            this.ResumeLayout(false);

        }

        #endregion        


        internal void SetMatrix(int x, int y, int p)
        {
            if (x < 0) x = 0;
            else if (x >= PathStore.MAPSPACE)
            {
                x = PathStore.MAPSPACE - 1;
            }
            if (y < 0) y = 0;
            else if (y >= PathStore.MAPSPACE)
            {
                y = PathStore.MAPSPACE - 1;
            }
            PathStore.mMatrix[x, y] = (byte)p;
            this.Invalidate(new Rectangle(x * mGridSize, TRANSPOSE(y) * mGridSize, mGridSize, mGridSize));
        }

        internal void SetStartEnd(Point S, Point E)
        {
            mStart = S;
            mEnd = E;
        }

        private delegate void DrawPathDelegate(List<PathFinderNode> path);
        internal void DrawPath(List<PathFinderNode> path)
        {
            if (path == null) return;
            if (this.InvokeRequired)
            {
                Invoke(new DrawPathDelegate(DrawPath), new object[] { path });
                return;
            }
            Graphics g = Graphics.FromHwnd(this.Handle);
            foreach (PathFinderNode N in path)
            {

                using (SolidBrush brush = new SolidBrush(Color.Red))
                    g.FillRectangle(brush, N.X * mGridSize, TRANSPOSE(N.Y) * mGridSize, mGridSize + 1, mGridSize + 1);
            }
        }
    }
}
