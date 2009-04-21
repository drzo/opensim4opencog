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
using System.IO;
using System.Data;
using System.Text;
using System.Drawing;
using System.Threading;
using System.Collections;
using System.Windows.Forms;
using System.ComponentModel;
using System.Collections.Generic;

using System.Globalization;
using PathSystem3D.Navigation;
namespace PathSystem3D.Navigation.Debug
{
    [Author("Franco, Gustavo")]
    internal partial class PathFinderDemo : Form
    {
        // public static PathFinderDemo Instance = new PathFinderDemo();
        #region Constants Declaration
        private const string STOP = "Stop";
        private const string RUN = "Run";
        private const string PAUSE = "Pause";
        private const string CONTINUE = "Continue";

        private const string NODE_START = "Start";
        private const string NODE_END = "End";
        private const string NODE_X = "X";
        private const string NODE_M = "X";
        public const int MAPSPACE_LS = 1000;
        #endregion

        #region Variables Declaration
        private Thread mPFThread = null;
        private IPathFinder mPathFinder = null;
        private int mDelay;
        private bool mPaused;
        private bool mRunning;
        #endregion

        #region Constuctors
        SimPathStore PathStore;
        public PathFinderDemo(SimPathStore pathStore)
        {
            PathStore = pathStore;
            //this.PnlGUI = new PanelPathFinder(pathStore);
            InitializeComponent();
            PnlGUI.SetPathStore(pathStore);
            CboFormula.SelectedIndex = 0;
            BtnStartStop.Text = RUN;
            BtnPause.Text = PAUSE;
            BtnStart.Text = NODE_START;
            BtnEnd.Text = NODE_END;
            BtnX.Text = NODE_X;
            mDelay = TBarSpeed.Value;
        }
        #endregion

        #region PathFinder Events
        private delegate void PathFinderDebugDelegate(int parentX, int parentY, int x, int y, PathFinderNodeType type, int totalCost, int cost);
        private void PathFinderDebug(int parentX, int parentY, int x, int y, PathFinderNodeType type, int totalCost, int cost)
        {
            if (InvokeRequired)
            {
                while (mPaused)
                    Thread.Sleep(100);

                Thread.Sleep(mDelay);
                Invoke(new PathFinderDebugDelegate(PathFinderDebug), new object[] { parentX, parentY, x, y, type, totalCost, cost });
                return;
            }

            PnlGUI.DrawDebug0(parentX, parentY, x, y, type, totalCost, cost);
        }
        #endregion

        #region Methods
        public void Run()
        {
            PnlGUI.Refresh();
            BtnStartStop.Text = STOP;
            ToolStrp.Enabled = false;
            PnlSettings.Enabled = false;
            BtnPause.Enabled = true;

            mPFThread = new Thread(new ThreadStart(RunPathFinder));
            mPFThread.Name = "Path Finder Thread";
            mPFThread.Start();
        }

        public void Stop()
        {
            if (mPathFinder != null)
                mPathFinder.FindPathStop();

            mPFThread.Abort();

            mPaused = false;
            BtnStartStop.Text = RUN;
            BtnPause.Text = PAUSE;
            ToolStrp.Enabled = true;
            PnlSettings.Enabled = true;
            BtnPause.Enabled = false;
        }


        private delegate void ShowDelegate();
        public new void Show()
        {
            if (this.InvokeRequired)
            {
                Invoke(new ShowDelegate(Reactivate), new object[] { });
                return;
            }
            Reactivate();
        }
        //public new void Activate()
        //{
        //    if (true || this.InvokeRequired)
        //    {
        //        Invoke(new ShowDelegate(Reactivate), new object[] { });
        //        return;
        //    }
        //    Reactivate();
        //}

        void Reactivate()
        {
            try
            {
                baseShow();

                //if (this.WindowState == FormWindowState.Minimized)              
                base.WindowState = FormWindowState.Normal;
                SimPathStore S = GetPathStore();
                this.Text = ""+S;
                if (!base.Visible) base.Visible = true;
                base.Activate();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }

        private void baseShow()
        {
            try
            {
                base.Show();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }

        private SimPathStore GetPathStore()
        {
            if (PathStore != null) return PathStore;
            return PnlGUI.PathStore;//.GetSimRegion();
        }

        private delegate void UpdateTimeLabelDelegate(double time);
        private void UpdateTimeLabel(double time)
        {
            if (this.InvokeRequired)
            {
                Invoke(new UpdateTimeLabelDelegate(UpdateTimeLabel), new object[] { time });
                return;
            }

            LblCompletedTimeValue.Text = time.ToString("N4");
        }


        public void RunPathFinder()
        {
            if (ChkUseFastPathFinder.Checked)
            {
                if (!(mPathFinder is PathFinderFasting))
                {
                    if (mPathFinder != null)
                        mPathFinder.PathFinderDebug -= new PathFinderDebugHandler(PathFinderDebug);

                    CollisionPlane CP = PnlGUI.CurrentPlane;
                    CP.EnsureUpToDate();

                    mPathFinder = new PathFinderFasting(CP.ByteMatrix);
                    mPathFinder.PathFinderDebug += new PathFinderDebugHandler(PathFinderDebug);
                }
            }
            else
            {
                if (!(mPathFinder is PathFinder))
                {
                    if (mPathFinder != null)
                        mPathFinder.PathFinderDebug -= new PathFinderDebugHandler(PathFinderDebug);


                    CollisionPlane CP = PnlGUI.CurrentPlane;
                    CP.EnsureUpToDate();
                    mPathFinder = new PathFinder(CP.ByteMatrix);
                    mPathFinder.PathFinderDebug += new PathFinderDebugHandler(PathFinderDebug);
                }
            }

            mPathFinder.Formula = PnlGUI.Formula;
            mPathFinder.Diagonals = ChkDiagonals.Checked;
            mPathFinder.HeavyDiagonals = ChkHeavyDiagonals.Checked;
            mPathFinder.HeuristicEstimate = (int)NumUpDownHeuristic.Value;
            mPathFinder.PunishChangeDirection = ChkPunishChangeDirection.Checked;
            mPathFinder.TieBreaker = ChkTieBraker.Checked;
            mPathFinder.SearchLimit = (int)NumSearchLimit.Value;
            mPathFinder.DebugProgress = ChlShowProgress.Checked;
            mPathFinder.ReopenCloseNodes = ChkReopenCloseNodes.Checked;
            mPathFinder.DebugFoundPath = true;

            List<PathFinderNode> path = mPathFinder.FindPath(PnlGUI.Start, PnlGUI.End);
            UpdateTimeLabel(mPathFinder.CompletedTime);

            if (path == null)
                MessageBox.Show("Path Not Found");

            PnlGUI.DrawPath(path);

            if (BtnStartStop.Text == STOP)
                BtnStartStop_Click(null, EventArgs.Empty);
        }
        #endregion

        #region Events
        private void TBarGridSize_Scroll(object sender, EventArgs e)
        {
            int GridSize = TBarGridSize.Value;
            PnlGUI.GridSize = GridSize;
        }

        private void TBarXY_Scroll(object sender, EventArgs e)
        {
            PnlGUI.GridX = TBarX.Value;
            PnlGUI.GridY = TBarY.Value;
            int GridSize = TBarGridSize.Value;
            PnlGUI.GridSize = GridSize;

        }

        private void TBarSpeed_Scroll(object sender, EventArgs e)
        {
            mDelay = (int)Math.Pow(TBarSpeed.Value, 2);
        }

        private delegate void BtnStartStop_ClickDelegate(object sender, EventArgs e);
        private void BtnStartStop_Click(object sender, EventArgs e)
        {
            if (InvokeRequired)
            {
                this.Invoke(new BtnStartStop_ClickDelegate(BtnStartStop_Click), new object[] { sender, e });
                return;
            }

            if (!mRunning)
                Run();
            else
                Stop();
            mRunning = !mRunning;
        }

        private void BtnPause_Click(object sender, EventArgs e)
        {
            if (!mPaused)
                BtnPause.Text = CONTINUE;
            else
                BtnPause.Text = PAUSE;
            mPaused = !mPaused;
        }

        private void Btn_Click(object sender, EventArgs e)
        {
            ToolStripButton btn = (ToolStripButton)sender;
            foreach (ToolStripItem item in ToolStrp.Items)
            {
                ToolStripButton toolButton = item as ToolStripButton;
                if (toolButton != null && toolButton.CheckOnClick)
                    toolButton.Checked = false;
            }
            btn.Checked = true;

            string text = btn.Tag as string;
            if (text == NODE_START)
                PnlGUI.DrawModeSetup = DrawModeSetup.Start;
            else if (text == NODE_END)
                PnlGUI.DrawModeSetup = DrawModeSetup.End;
            else if (text == NODE_X)
            {
                PnlGUI.NodeWeight = 0;
                PnlGUI.DrawModeSetup = DrawModeSetup.Block;
            }
            else
            {
                PnlGUI.NodeWeight = byte.Parse(text);
                PnlGUI.DrawModeSetup = DrawModeSetup.Block;
            }
        }


        private void CboFormula_SelectedIndexChanged(object sender, EventArgs e)
        {
            PnlGUI.Formula = (HeuristicFormula)CboFormula.SelectedIndex + 1;
        }

        private void BtnNew_Click(object sender, EventArgs e)
        {
            PnlGUI.ResetMatrix();
            PnlGUI.Invalidate();
        }

        private void BtnSave_Click(object sender, EventArgs e)
        {
            SaveDLG.InitialDirectory = Application.StartupPath;

            if (SaveDLG.ShowDialog(this) == DialogResult.OK)
            {
                if (SaveDLG.FileName.Length == 0)
                    return;

                FileStream fs = new FileStream(SaveDLG.FileName, FileMode.Create, FileAccess.Write);

                fs.WriteByte((byte)(PnlGUI.Start.X >> 8));
                fs.WriteByte((byte)(PnlGUI.Start.X & 0x000000FF));
                fs.WriteByte((byte)(PnlGUI.Start.Y >> 8));
                fs.WriteByte((byte)(PnlGUI.Start.Y & 0x000000FF));
                fs.WriteByte((byte)(PnlGUI.End.X >> 8));
                fs.WriteByte((byte)(PnlGUI.End.X & 0x000000FF));
                fs.WriteByte((byte)(PnlGUI.End.Y >> 8));
                fs.WriteByte((byte)(PnlGUI.End.Y & 0x000000FF));
                fs.WriteByte((byte)(ChkDiagonals.Checked ? 1 : 0));
                fs.WriteByte((byte)(ChkHeavyDiagonals.Checked ? 1 : 0));
                fs.WriteByte((byte)(ChkPunishChangeDirection.Checked ? 1 : 0));
                fs.WriteByte((byte)(ChkReopenCloseNodes.Checked ? 1 : 0));
                fs.WriteByte((byte)NumUpDownHeuristic.Value);
                fs.WriteByte((byte)CboFormula.SelectedIndex);
                fs.WriteByte((byte)(ChkTieBraker.Checked ? 1 : 0));
                fs.WriteByte((byte)(((int)NumSearchLimit.Value) >> 24));
                fs.WriteByte((byte)(((int)NumSearchLimit.Value) >> 16));
                fs.WriteByte((byte)(((int)NumSearchLimit.Value) >> 8));
                fs.WriteByte((byte)(((int)NumSearchLimit.Value) & 0x000000FF));
                fs.WriteByte((byte)TBarGridSize.Value);
                fs.WriteByte((byte)TBarSpeed.Value);

                for (int y = 0; y < MAPSPACE_LS; y++)
                    for (int x = 0; x < MAPSPACE_LS; x++)
                        fs.WriteByte(PnlGUI.Matrix[x, y]);

                fs.Close();
            }
        }

        private void BtnLoad_Click(object sender, EventArgs e)
        {
            if (OpenDLG.ShowDialog(this) == DialogResult.OK)
            {
                if (OpenDLG.FileName.Length == 0)
                    return;

                FileStream fs = new FileStream(OpenDLG.FileName, FileMode.Open, FileAccess.Read);
                if (fs.Length != 1000021)
                {
                    MessageBox.Show(this, "Invalid File", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }

                PnlGUI.DrawModeSetup = DrawModeSetup.None;
                foreach (ToolStripItem item in ToolStrp.Items)
                {
                    if (item is ToolStripButton)
                        ((ToolStripButton)item).Checked = false;
                }

                PnlGUI.Start = new Point(fs.ReadByte() << 8 | fs.ReadByte(), fs.ReadByte() << 8 | fs.ReadByte());
                PnlGUI.End = new Point(fs.ReadByte() << 8 | fs.ReadByte(), fs.ReadByte() << 8 | fs.ReadByte());
                ChkDiagonals.Checked = fs.ReadByte() > 0;
                ChkHeavyDiagonals.Checked = fs.ReadByte() > 0;
                ChkPunishChangeDirection.Checked = fs.ReadByte() > 0;
                ChkReopenCloseNodes.Checked = fs.ReadByte() > 0;
                NumUpDownHeuristic.Value = fs.ReadByte();
                CboFormula.SelectedIndex = fs.ReadByte();
                ChkTieBraker.Checked = fs.ReadByte() > 0;
                NumSearchLimit.Value = fs.ReadByte() << 24 | fs.ReadByte() << 16 | fs.ReadByte() << 8 | fs.ReadByte();
                TBarGridSize.Value = fs.ReadByte();
                PnlGUI.GridSize = TBarGridSize.Value;
                TBarSpeed.Value = fs.ReadByte();
                mDelay = (int)Math.Pow(TBarSpeed.Value, 2);

                for (int y = 0; y < MAPSPACE_LS; y++)
                    for (int x = 0; x < MAPSPACE_LS; x++)
                        PnlGUI.Matrix[x, y] = (byte)fs.ReadByte();

                fs.Close();
                PnlGUI.Invalidate();
            }

        }
        #endregion

        #region Overrides
        protected override void OnClosing(CancelEventArgs e)
        {
            if (BtnStartStop.Text == STOP)
                BtnStartStop_Click(null, EventArgs.Empty);
            base.OnClosing(e);
        }
        #endregion


        internal void SetStartEnd(Point S, Point E)
        {
            PnlGUI.SetStartEnd(S, E);
        }

        internal void ShowPath(IList<PathFinderNode> pfn)
        {
            PnlGUI.DrawPath(pfn);
        }

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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(PathFinderDemo));
            this.PnlGUI = new PathSystem3D.Navigation.Debug.PanelPathFinder();
            this.ToolStrp = new System.Windows.Forms.ToolStrip();
            this.BtnNew = new System.Windows.Forms.ToolStripButton();
            this.BtnLoad = new System.Windows.Forms.ToolStripButton();
            this.BtnSave = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator10 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn1 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn2 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn5 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn10 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator11 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn13 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn20 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn40 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.Btn80 = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator12 = new System.Windows.Forms.ToolStripSeparator();
            this.ButtonM = new System.Windows.Forms.ToolStripButton();
            this.BtnX = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
            this.BtnStart = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator8 = new System.Windows.Forms.ToolStripSeparator();
            this.BtnEnd = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator9 = new System.Windows.Forms.ToolStripSeparator();
            this.SaveDLG = new System.Windows.Forms.SaveFileDialog();
            this.OpenDLG = new System.Windows.Forms.OpenFileDialog();
            this.TBarSpeed = new System.Windows.Forms.TrackBar();
            this.LblSpeed = new System.Windows.Forms.Label();
            this.ChkDiagonals = new System.Windows.Forms.CheckBox();
            this.ChkPunishChangeDirection = new System.Windows.Forms.CheckBox();
            this.NumUpDownHeuristic = new System.Windows.Forms.NumericUpDown();
            this.LblHeuristic = new System.Windows.Forms.Label();
            this.BtnPause = new System.Windows.Forms.Button();
            this.CboFormula = new System.Windows.Forms.ComboBox();
            this.LblFormula = new System.Windows.Forms.Label();
            this.ChkTieBraker = new System.Windows.Forms.CheckBox();
            this.BtnStartStop = new System.Windows.Forms.Button();
            this.PnlSettings = new System.Windows.Forms.Panel();
            this.MinZevel = new System.Windows.Forms.TextBox();
            this.BtnRecomputeMatrix = new System.Windows.Forms.Button();
            this.BtnRebakeTerrain = new System.Windows.Forms.Button();
            this.ChkReopenCloseNodes = new System.Windows.Forms.CheckBox();
            this.ChkUseFastPathFinder = new System.Windows.Forms.CheckBox();
            this.ChlShowProgress = new System.Windows.Forms.CheckBox();
            this.ChkHeavyDiagonals = new System.Windows.Forms.CheckBox();
            this.LblGridSize = new System.Windows.Forms.Label();
            this.NumSearchLimit = new System.Windows.Forms.NumericUpDown();
            this.TBarGridSize = new System.Windows.Forms.TrackBar();
            this.LblSearchLimit = new System.Windows.Forms.Label();
            this.TBarX = new System.Windows.Forms.TrackBar();
            this.TBarY = new System.Windows.Forms.TrackBar();
            this.LblCompletedTimeValue = new System.Windows.Forms.Label();
            this.LblCompletedTime = new System.Windows.Forms.Label();
            this.CollisionPlaneList = new System.Windows.Forms.ComboBox();
            this.ToolStrp.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.TBarSpeed)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.NumUpDownHeuristic)).BeginInit();
            this.PnlSettings.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.NumSearchLimit)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarGridSize)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarX)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarY)).BeginInit();
            this.SuspendLayout();
            // 
            // PnlGUI
            // 
            this.PnlGUI.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.PnlGUI.BackColor = System.Drawing.Color.White;
            this.PnlGUI.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.PnlGUI.DrawModeSetup = PathSystem3D.Navigation.Debug.DrawModeSetup.None;
            this.PnlGUI.End = new System.Drawing.Point(0, 0);
            this.PnlGUI.Formula = PathSystem3D.Navigation.HeuristicFormula.Manhattan;
            this.PnlGUI.GridSize = 1;
            this.PnlGUI.Location = new System.Drawing.Point(-1, -1);
            this.PnlGUI.Name = "PnlGUI";
            this.PnlGUI.NodeWeight = ((byte)(1));
            this.PnlGUI.Size = new System.Drawing.Size(664, 611);
            this.PnlGUI.Start = new System.Drawing.Point(1, 1);
            this.PnlGUI.TabIndex = 1;
            // this.PnlGUI.ZLevel = 0F;
            // 
            // ToolStrp
            // 
            this.ToolStrp.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.ToolStrp.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.BtnNew,
            this.BtnLoad,
            this.BtnSave,
            this.toolStripSeparator10,
            this.Btn1,
            this.toolStripSeparator1,
            this.Btn2,
            this.toolStripSeparator2,
            this.Btn5,
            this.toolStripSeparator3,
            this.Btn10,
            this.toolStripSeparator11,
            this.Btn13,
            this.toolStripSeparator4,
            this.Btn20,
            this.toolStripSeparator5,
            this.Btn40,
            this.toolStripSeparator6,
            this.Btn80,
            this.toolStripSeparator12,
            this.ButtonM,
            this.BtnX,
            this.toolStripSeparator7,
            this.BtnStart,
            this.toolStripSeparator8,
            this.BtnEnd,
            this.toolStripSeparator9});
            this.ToolStrp.Location = new System.Drawing.Point(0, 620);
            this.ToolStrp.Name = "ToolStrp";
            this.ToolStrp.Size = new System.Drawing.Size(824, 25);
            this.ToolStrp.TabIndex = 20;
            this.ToolStrp.Text = "toolStrip1";
            // 
            // BtnNew
            // 
            this.BtnNew.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.BtnNew.Image = ((System.Drawing.Image)(resources.GetObject("BtnNew.Image")));
            this.BtnNew.ImageTransparentColor = System.Drawing.Color.Black;
            this.BtnNew.Name = "BtnNew";
            this.BtnNew.Size = new System.Drawing.Size(23, 22);
            this.BtnNew.Text = this.BtnNew.ToolTipText;
            this.BtnNew.ToolTipText = "New";
            this.BtnNew.Click += new System.EventHandler(this.BtnNew_Click);
            // 
            // BtnLoad
            // 
            this.BtnLoad.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.BtnLoad.Image = ((System.Drawing.Image)(resources.GetObject("BtnLoad.Image")));
            this.BtnLoad.ImageTransparentColor = System.Drawing.Color.Black;
            this.BtnLoad.Name = "BtnLoad";
            this.BtnLoad.Size = new System.Drawing.Size(23, 22);
            this.BtnLoad.Text = this.BtnLoad.ToolTipText;
            this.BtnLoad.ToolTipText = "Load";
            this.BtnLoad.Click += new System.EventHandler(this.BtnLoad_Click);
            // 
            // BtnSave
            // 
            this.BtnSave.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
          //  this.BtnSave.Image = global::cogbot.Properties.Resources.Sauver;
            this.BtnSave.ImageTransparentColor = System.Drawing.Color.Black;
            this.BtnSave.Name = "BtnSave";
            this.BtnSave.Size = new System.Drawing.Size(23, 22);
            this.BtnSave.Text = this.BtnSave.ToolTipText;
            this.BtnSave.ToolTipText = "Save";
            this.BtnSave.Click += new System.EventHandler(this.BtnSave_Click);
            // 
            // toolStripSeparator10
            // 
            this.toolStripSeparator10.Name = "toolStripSeparator10";
            this.toolStripSeparator10.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn1
            // 
            this.Btn1.CheckOnClick = true;
            this.Btn1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn1.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn1.Name = "Btn1";
            this.Btn1.Size = new System.Drawing.Size(23, 22);
            this.Btn1.Tag = "1";
            this.Btn1.Text = "1";
            this.Btn1.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn2
            // 
            this.Btn2.CheckOnClick = true;
            this.Btn2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn2.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn2.Name = "Btn2";
            this.Btn2.Size = new System.Drawing.Size(23, 22);
            this.Btn2.Tag = "2";
            this.Btn2.Text = "2";
            this.Btn2.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn5
            // 
            this.Btn5.CheckOnClick = true;
            this.Btn5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn5.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn5.Name = "Btn5";
            this.Btn5.Size = new System.Drawing.Size(23, 22);
            this.Btn5.Tag = "5";
            this.Btn5.Text = "5";
            this.Btn5.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn10
            // 
            this.Btn10.CheckOnClick = true;
            this.Btn10.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn10.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn10.Name = "Btn10";
            this.Btn10.Size = new System.Drawing.Size(23, 22);
            this.Btn10.Tag = "10";
            this.Btn10.Text = "10";
            this.Btn10.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator11
            // 
            this.toolStripSeparator11.Name = "toolStripSeparator11";
            this.toolStripSeparator11.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn13
            // 
            this.Btn13.CheckOnClick = true;
            this.Btn13.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn13.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn13.Name = "Btn13";
            this.Btn13.Size = new System.Drawing.Size(23, 22);
            this.Btn13.Tag = "13";
            this.Btn13.Text = "13";
            this.Btn13.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn20
            // 
            this.Btn20.CheckOnClick = true;
            this.Btn20.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn20.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn20.Name = "Btn20";
            this.Btn20.Size = new System.Drawing.Size(23, 22);
            this.Btn20.Tag = "20";
            this.Btn20.Text = "20";
            this.Btn20.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn40
            // 
            this.Btn40.CheckOnClick = true;
            this.Btn40.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn40.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn40.Name = "Btn40";
            this.Btn40.Size = new System.Drawing.Size(23, 22);
            this.Btn40.Tag = "40";
            this.Btn40.Text = "40";
            this.Btn40.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(6, 25);
            // 
            // Btn80
            // 
            this.Btn80.CheckOnClick = true;
            this.Btn80.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.Btn80.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.Btn80.Name = "Btn80";
            this.Btn80.Size = new System.Drawing.Size(23, 22);
            this.Btn80.Tag = "80";
            this.Btn80.Text = "80";
            this.Btn80.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator12
            // 
            this.toolStripSeparator12.Name = "toolStripSeparator12";
            this.toolStripSeparator12.Size = new System.Drawing.Size(6, 25);
            // 
            // ButtonM
            // 
            this.ButtonM.CheckOnClick = true;
            this.ButtonM.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ButtonM.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ButtonM.Name = "ButtonM";
            this.ButtonM.Size = new System.Drawing.Size(23, 22);
            this.ButtonM.Tag = "0";
            this.ButtonM.Text = "M";
            this.ButtonM.ToolTipText = "M";
            // 
            // BtnX
            // 
            this.BtnX.CheckOnClick = true;
            this.BtnX.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.BtnX.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.BtnX.Name = "BtnX";
            this.BtnX.Size = new System.Drawing.Size(23, 22);
            this.BtnX.Tag = "0";
            this.BtnX.Text = "X";
            this.BtnX.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator7
            // 
            this.toolStripSeparator7.Name = "toolStripSeparator7";
            this.toolStripSeparator7.Size = new System.Drawing.Size(6, 25);
            // 
            // BtnStart
            // 
            this.BtnStart.CheckOnClick = true;
            this.BtnStart.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.BtnStart.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.BtnStart.Name = "BtnStart";
            this.BtnStart.Size = new System.Drawing.Size(35, 22);
            this.BtnStart.Tag = "Start";
            this.BtnStart.Text = "Start";
            this.BtnStart.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator8
            // 
            this.toolStripSeparator8.Name = "toolStripSeparator8";
            this.toolStripSeparator8.Size = new System.Drawing.Size(6, 25);
            // 
            // BtnEnd
            // 
            this.BtnEnd.CheckOnClick = true;
            this.BtnEnd.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.BtnEnd.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.BtnEnd.Name = "BtnEnd";
            this.BtnEnd.Size = new System.Drawing.Size(29, 22);
            this.BtnEnd.Tag = "End";
            this.BtnEnd.Text = "End";
            this.BtnEnd.Click += new System.EventHandler(this.Btn_Click);
            // 
            // toolStripSeparator9
            // 
            this.toolStripSeparator9.Name = "toolStripSeparator9";
            this.toolStripSeparator9.Size = new System.Drawing.Size(6, 25);
            // 
            // SaveDLG
            // 
            this.SaveDLG.DefaultExt = "astar";
            this.SaveDLG.Filter = "Map files|*.astar";
            this.SaveDLG.RestoreDirectory = true;
            // 
            // OpenDLG
            // 
            this.OpenDLG.DefaultExt = "astar";
            this.OpenDLG.Filter = "Map files|*.astar";
            this.OpenDLG.RestoreDirectory = true;
            // 
            // TBarSpeed
            // 
            this.TBarSpeed.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.TBarSpeed.AutoSize = false;
            this.TBarSpeed.LargeChange = 10;
            this.TBarSpeed.Location = new System.Drawing.Point(674, 24);
            this.TBarSpeed.Maximum = 60;
            this.TBarSpeed.Name = "TBarSpeed";
            this.TBarSpeed.Size = new System.Drawing.Size(121, 33);
            this.TBarSpeed.TabIndex = 2;
            this.TBarSpeed.TickFrequency = 10;
            this.TBarSpeed.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarSpeed.Value = 3;
            this.TBarSpeed.Scroll += new System.EventHandler(this.TBarSpeed_Scroll);
            // 
            // LblSpeed
            // 
            this.LblSpeed.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.LblSpeed.AutoSize = true;
            this.LblSpeed.BackColor = System.Drawing.Color.Transparent;
            this.LblSpeed.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.LblSpeed.Location = new System.Drawing.Point(676, 9);
            this.LblSpeed.Name = "LblSpeed";
            this.LblSpeed.Size = new System.Drawing.Size(38, 13);
            this.LblSpeed.TabIndex = 12;
            this.LblSpeed.Text = "Speed";
            this.LblSpeed.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // ChkDiagonals
            // 
            this.ChkDiagonals.AutoSize = true;
            this.ChkDiagonals.Checked = true;
            this.ChkDiagonals.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ChkDiagonals.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkDiagonals.Location = new System.Drawing.Point(12, 4);
            this.ChkDiagonals.Name = "ChkDiagonals";
            this.ChkDiagonals.Size = new System.Drawing.Size(73, 17);
            this.ChkDiagonals.TabIndex = 3;
            this.ChkDiagonals.Text = "Diagonals";
            this.ChkDiagonals.UseVisualStyleBackColor = true;
            // 
            // ChkPunishChangeDirection
            // 
            this.ChkPunishChangeDirection.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkPunishChangeDirection.Location = new System.Drawing.Point(12, 47);
            this.ChkPunishChangeDirection.Name = "ChkPunishChangeDirection";
            this.ChkPunishChangeDirection.Size = new System.Drawing.Size(140, 30);
            this.ChkPunishChangeDirection.TabIndex = 5;
            this.ChkPunishChangeDirection.Text = "Punish Change Direction";
            this.ChkPunishChangeDirection.UseVisualStyleBackColor = true;
            // 
            // NumUpDownHeuristic
            // 
            this.NumUpDownHeuristic.Location = new System.Drawing.Point(12, 114);
            this.NumUpDownHeuristic.Name = "NumUpDownHeuristic";
            this.NumUpDownHeuristic.Size = new System.Drawing.Size(38, 20);
            this.NumUpDownHeuristic.TabIndex = 7;
            this.NumUpDownHeuristic.Value = new decimal(new int[] {
            2,
            0,
            0,
            0});
            // 
            // LblHeuristic
            // 
            this.LblHeuristic.AutoSize = true;
            this.LblHeuristic.BackColor = System.Drawing.Color.Transparent;
            this.LblHeuristic.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.LblHeuristic.Location = new System.Drawing.Point(9, 101);
            this.LblHeuristic.Name = "LblHeuristic";
            this.LblHeuristic.Size = new System.Drawing.Size(48, 13);
            this.LblHeuristic.TabIndex = 16;
            this.LblHeuristic.Text = "Heuristic";
            this.LblHeuristic.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // BtnPause
            // 
            this.BtnPause.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.BtnPause.Enabled = false;
            this.BtnPause.Location = new System.Drawing.Point(673, 587);
            this.BtnPause.Name = "BtnPause";
            this.BtnPause.Size = new System.Drawing.Size(143, 23);
            this.BtnPause.TabIndex = 19;
            this.BtnPause.Text = "Pause";
            this.BtnPause.UseVisualStyleBackColor = true;
            this.BtnPause.Click += new System.EventHandler(this.BtnPause_Click);
            // 
            // CboFormula
            // 
            this.CboFormula.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.CboFormula.FormattingEnabled = true;
            this.CboFormula.Items.AddRange(new object[] {
            "Manhattan",
            "Max(DX,DY)",
            "Diagonal Shortcut",
            "Euclidean",
            "Euclidean No SQR",
            "Custom"});
            this.CboFormula.Location = new System.Drawing.Point(12, 150);
            this.CboFormula.Name = "CboFormula";
            this.CboFormula.Size = new System.Drawing.Size(137, 21);
            this.CboFormula.TabIndex = 8;
            this.CboFormula.SelectedIndexChanged += new System.EventHandler(this.CboFormula_SelectedIndexChanged);
            // 
            // LblFormula
            // 
            this.LblFormula.AutoSize = true;
            this.LblFormula.BackColor = System.Drawing.Color.Transparent;
            this.LblFormula.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.LblFormula.Location = new System.Drawing.Point(9, 137);
            this.LblFormula.Name = "LblFormula";
            this.LblFormula.Size = new System.Drawing.Size(44, 13);
            this.LblFormula.TabIndex = 19;
            this.LblFormula.Text = "Formula";
            this.LblFormula.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // ChkTieBraker
            // 
            this.ChkTieBraker.AutoSize = true;
            this.ChkTieBraker.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ChkTieBraker.Location = new System.Drawing.Point(12, 177);
            this.ChkTieBraker.Name = "ChkTieBraker";
            this.ChkTieBraker.Size = new System.Drawing.Size(103, 17);
            this.ChkTieBraker.TabIndex = 9;
            this.ChkTieBraker.Text = "Use Tie Breaker";
            this.ChkTieBraker.UseVisualStyleBackColor = true;
            // 
            // BtnStartStop
            // 
            this.BtnStartStop.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.BtnStartStop.Location = new System.Drawing.Point(673, 558);
            this.BtnStartStop.Name = "BtnStartStop";
            this.BtnStartStop.Size = new System.Drawing.Size(143, 23);
            this.BtnStartStop.TabIndex = 18;
            this.BtnStartStop.Text = "Run";
            this.BtnStartStop.UseVisualStyleBackColor = true;
            this.BtnStartStop.Click += new System.EventHandler(this.BtnStartStop_Click);
            // 
            // PnlSettings
            // 
            this.PnlSettings.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.PnlSettings.Controls.Add(this.CollisionPlaneList);
            this.PnlSettings.Controls.Add(this.MinZevel);
            this.PnlSettings.Controls.Add(this.BtnRecomputeMatrix);
            this.PnlSettings.Controls.Add(this.BtnRebakeTerrain);
            this.PnlSettings.Controls.Add(this.ChkReopenCloseNodes);
            this.PnlSettings.Controls.Add(this.ChkUseFastPathFinder);
            this.PnlSettings.Controls.Add(this.ChlShowProgress);
            this.PnlSettings.Controls.Add(this.ChkHeavyDiagonals);
            this.PnlSettings.Controls.Add(this.LblGridSize);
            this.PnlSettings.Controls.Add(this.NumSearchLimit);
            this.PnlSettings.Controls.Add(this.TBarGridSize);
            this.PnlSettings.Controls.Add(this.LblSearchLimit);
            this.PnlSettings.Controls.Add(this.ChkDiagonals);
            this.PnlSettings.Controls.Add(this.ChkTieBraker);
            this.PnlSettings.Controls.Add(this.ChkPunishChangeDirection);
            this.PnlSettings.Controls.Add(this.LblFormula);
            this.PnlSettings.Controls.Add(this.NumUpDownHeuristic);
            this.PnlSettings.Controls.Add(this.CboFormula);
            this.PnlSettings.Controls.Add(this.LblHeuristic);
            this.PnlSettings.Controls.Add(this.TBarX);
            this.PnlSettings.Controls.Add(this.TBarY);
            this.PnlSettings.Location = new System.Drawing.Point(669, 59);
            this.PnlSettings.Name = "PnlSettings";
            this.PnlSettings.Size = new System.Drawing.Size(155, 534);
            this.PnlSettings.TabIndex = 17;
            // 
            // MinZevel
            // 
            this.MinZevel.Location = new System.Drawing.Point(15, 416);
            this.MinZevel.Name = "MinZevel";
            this.MinZevel.Size = new System.Drawing.Size(59, 20);
            this.MinZevel.TabIndex = 26;
            this.MinZevel.TextChanged += new System.EventHandler(this.MinZevel_TextChanged);
            // 
            // BtnRecomputeMatrix
            // 
            this.BtnRecomputeMatrix.Location = new System.Drawing.Point(5, 443);
            this.BtnRecomputeMatrix.Name = "BtnRecomputeMatrix";
            this.BtnRecomputeMatrix.Size = new System.Drawing.Size(137, 22);
            this.BtnRecomputeMatrix.TabIndex = 16;
            this.BtnRecomputeMatrix.Text = "Recompute Matrix";
            this.BtnRecomputeMatrix.UseVisualStyleBackColor = true;
            this.BtnRecomputeMatrix.Click += new System.EventHandler(this.BtnRecomputeMatrix_Click);
            // 
            // BtnRebakeTerrain
            // 
            this.BtnRebakeTerrain.Location = new System.Drawing.Point(6, 471);
            this.BtnRebakeTerrain.Name = "BtnRebakeTerrain";
            this.BtnRebakeTerrain.Size = new System.Drawing.Size(137, 22);
            this.BtnRebakeTerrain.TabIndex = 17;
            this.BtnRebakeTerrain.Text = "Rebake Terrain";
            this.BtnRebakeTerrain.UseVisualStyleBackColor = true;
            this.BtnRebakeTerrain.Click += new System.EventHandler(this.BtnRebakeTerrain_Click);
            // 
            // ChkReopenCloseNodes
            // 
            this.ChkReopenCloseNodes.Checked = true;
            this.ChkReopenCloseNodes.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ChkReopenCloseNodes.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkReopenCloseNodes.Location = new System.Drawing.Point(12, 78);
            this.ChkReopenCloseNodes.Name = "ChkReopenCloseNodes";
            this.ChkReopenCloseNodes.Size = new System.Drawing.Size(140, 20);
            this.ChkReopenCloseNodes.TabIndex = 6;
            this.ChkReopenCloseNodes.Text = "Reopen Closed Nodes";
            this.ChkReopenCloseNodes.UseVisualStyleBackColor = true;
            // 
            // ChkUseFastPathFinder
            // 
            this.ChkUseFastPathFinder.Checked = true;
            this.ChkUseFastPathFinder.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ChkUseFastPathFinder.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkUseFastPathFinder.Location = new System.Drawing.Point(12, 288);
            this.ChkUseFastPathFinder.Name = "ChkUseFastPathFinder";
            this.ChkUseFastPathFinder.Size = new System.Drawing.Size(137, 20);
            this.ChkUseFastPathFinder.TabIndex = 12;
            this.ChkUseFastPathFinder.Text = "Fast PathFinder";
            this.ChkUseFastPathFinder.UseVisualStyleBackColor = true;
            // 
            // ChlShowProgress
            // 
            this.ChlShowProgress.AutoSize = true;
            this.ChlShowProgress.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChlShowProgress.Location = new System.Drawing.Point(12, 311);
            this.ChlShowProgress.Name = "ChlShowProgress";
            this.ChlShowProgress.Size = new System.Drawing.Size(97, 17);
            this.ChlShowProgress.TabIndex = 13;
            this.ChlShowProgress.Text = "Show Progress";
            this.ChlShowProgress.UseVisualStyleBackColor = true;
            // 
            // ChkHeavyDiagonals
            // 
            this.ChkHeavyDiagonals.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkHeavyDiagonals.Location = new System.Drawing.Point(12, 24);
            this.ChkHeavyDiagonals.Name = "ChkHeavyDiagonals";
            this.ChkHeavyDiagonals.Size = new System.Drawing.Size(140, 20);
            this.ChkHeavyDiagonals.TabIndex = 4;
            this.ChkHeavyDiagonals.Text = "Heavy Diagonals";
            this.ChkHeavyDiagonals.UseVisualStyleBackColor = true;
            // 
            // LblGridSize
            // 
            this.LblGridSize.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.LblGridSize.AutoSize = true;
            this.LblGridSize.BackColor = System.Drawing.Color.Transparent;
            this.LblGridSize.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.LblGridSize.Location = new System.Drawing.Point(9, 233);
            this.LblGridSize.Name = "LblGridSize";
            this.LblGridSize.Size = new System.Drawing.Size(49, 13);
            this.LblGridSize.TabIndex = 25;
            this.LblGridSize.Text = "Grid Size";
            this.LblGridSize.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // NumSearchLimit
            // 
            this.NumSearchLimit.Location = new System.Drawing.Point(12, 210);
            this.NumSearchLimit.Maximum = new decimal(new int[] {
            999999,
            0,
            0,
            0});
            this.NumSearchLimit.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.NumSearchLimit.Name = "NumSearchLimit";
            this.NumSearchLimit.Size = new System.Drawing.Size(62, 20);
            this.NumSearchLimit.TabIndex = 10;
            this.NumSearchLimit.Value = new decimal(new int[] {
            50000,
            0,
            0,
            0});
            // 
            // TBarGridSize
            // 
            this.TBarGridSize.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.TBarGridSize.AutoSize = false;
            this.TBarGridSize.LargeChange = 4;
            this.TBarGridSize.Location = new System.Drawing.Point(5, 249);
            this.TBarGridSize.Maximum = 20;
            this.TBarGridSize.Minimum = 1;
            this.TBarGridSize.Name = "TBarGridSize";
            this.TBarGridSize.Size = new System.Drawing.Size(144, 33);
            this.TBarGridSize.TabIndex = 11;
            this.TBarGridSize.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarGridSize.Value = 2;
            this.TBarGridSize.Scroll += new System.EventHandler(this.TBarGridSize_Scroll);
            // 
            // LblSearchLimit
            // 
            this.LblSearchLimit.AutoSize = true;
            this.LblSearchLimit.BackColor = System.Drawing.Color.Transparent;
            this.LblSearchLimit.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.LblSearchLimit.Location = new System.Drawing.Point(9, 197);
            this.LblSearchLimit.Name = "LblSearchLimit";
            this.LblSearchLimit.Size = new System.Drawing.Size(65, 13);
            this.LblSearchLimit.TabIndex = 21;
            this.LblSearchLimit.Text = "Search Limit";
            this.LblSearchLimit.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // TBarX
            // 
            this.TBarX.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.TBarX.AutoSize = false;
            this.TBarX.Location = new System.Drawing.Point(5, 353);
            this.TBarX.Maximum = 254;
            this.TBarX.Name = "TBarX";
            this.TBarX.Size = new System.Drawing.Size(144, 33);
            this.TBarX.TabIndex = 14;
            this.TBarX.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarX.Value = 1;
            this.TBarX.Scroll += new System.EventHandler(this.TBarXY_Scroll);
            // 
            // TBarY
            // 
            this.TBarY.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.TBarY.AutoSize = false;
            this.TBarY.Location = new System.Drawing.Point(4, 377);
            this.TBarY.Maximum = 254;
            this.TBarY.Name = "TBarY";
            this.TBarY.Size = new System.Drawing.Size(144, 33);
            this.TBarY.TabIndex = 15;
            this.TBarY.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarY.Value = 1;
            this.TBarY.Scroll += new System.EventHandler(this.TBarXY_Scroll);
            // 
            // LblCompletedTimeValue
            // 
            this.LblCompletedTimeValue.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.LblCompletedTimeValue.Location = new System.Drawing.Point(752, 396);
            this.LblCompletedTimeValue.Margin = new System.Windows.Forms.Padding(0);
            this.LblCompletedTimeValue.Name = "LblCompletedTimeValue";
            this.LblCompletedTimeValue.Size = new System.Drawing.Size(44, 13);
            this.LblCompletedTimeValue.TabIndex = 28;
            this.LblCompletedTimeValue.Text = "0.000,0";
            // 
            // LblCompletedTime
            // 
            this.LblCompletedTime.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.LblCompletedTime.AutoSize = true;
            this.LblCompletedTime.Location = new System.Drawing.Point(672, 396);
            this.LblCompletedTime.Name = "LblCompletedTime";
            this.LblCompletedTime.Size = new System.Drawing.Size(148, 13);
            this.LblCompletedTime.TabIndex = 27;
            this.LblCompletedTime.Text = "Completed Time               sec.";
            // 
            // CollisionPlaneList
            // 
            this.CollisionPlaneList.FormattingEnabled = true;
            this.CollisionPlaneList.Location = new System.Drawing.Point(81, 416);
            this.CollisionPlaneList.Name = "CollisionPlaneList";
            this.CollisionPlaneList.Size = new System.Drawing.Size(61, 21);
            this.CollisionPlaneList.TabIndex = 27;
            this.CollisionPlaneList.SelectedIndexChanged += new System.EventHandler(this.CollisionPlaneList_SelectedIndexChanged);
            // 
            // PathFinderDemo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(824, 645);
            this.Controls.Add(this.LblSpeed);
            this.Controls.Add(this.LblCompletedTimeValue);
            this.Controls.Add(this.LblCompletedTime);
            this.Controls.Add(this.TBarSpeed);
            this.Controls.Add(this.BtnStartStop);
            this.Controls.Add(this.PnlGUI);
            this.Controls.Add(this.ToolStrp);
            this.Controls.Add(this.BtnPause);
            this.Controls.Add(this.PnlSettings);
            this.MinimumSize = new System.Drawing.Size(500, 527);
            this.Name = "PathFinderDemo";
            this.Text = "AStar Demo";
            this.ToolStrp.ResumeLayout(false);
            this.ToolStrp.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.TBarSpeed)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.NumUpDownHeuristic)).EndInit();
            this.PnlSettings.ResumeLayout(false);
            this.PnlSettings.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.NumSearchLimit)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarGridSize)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarX)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.TBarY)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStrip ToolStrp;
        private System.Windows.Forms.ToolStripButton Btn1;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton Btn2;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripButton Btn5;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripButton Btn10;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        private System.Windows.Forms.ToolStripButton Btn20;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        private System.Windows.Forms.ToolStripButton Btn40;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
        private System.Windows.Forms.ToolStripButton Btn80;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator7;
        private System.Windows.Forms.ToolStripButton BtnStart;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator8;
        private System.Windows.Forms.ToolStripButton BtnEnd;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator9;
        private System.Windows.Forms.ToolStripButton BtnLoad;
        private System.Windows.Forms.ToolStripButton BtnSave;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator10;
        private System.Windows.Forms.SaveFileDialog SaveDLG;
        private System.Windows.Forms.OpenFileDialog OpenDLG;
        private System.Windows.Forms.TrackBar TBarSpeed;
        private System.Windows.Forms.Label LblSpeed;
        private System.Windows.Forms.ToolStripButton BtnNew;
        private System.Windows.Forms.CheckBox ChkDiagonals;
        private System.Windows.Forms.CheckBox ChkPunishChangeDirection;
        private System.Windows.Forms.NumericUpDown NumUpDownHeuristic;
        private System.Windows.Forms.Label LblHeuristic;
        private System.Windows.Forms.Button BtnPause;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator11;
        private System.Windows.Forms.ToolStripButton Btn13;
        private System.Windows.Forms.ComboBox CboFormula;
        private System.Windows.Forms.Label LblFormula;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator12;
        private System.Windows.Forms.ToolStripButton BtnX;
        private System.Windows.Forms.ToolStripButton ButtonM;
        private System.Windows.Forms.CheckBox ChkTieBraker;
        private PanelPathFinder PnlGUI;
        private System.Windows.Forms.Button BtnStartStop;
        private System.Windows.Forms.Button BtnRecomputeMatrix;
        private System.Windows.Forms.Button BtnRebakeTerrain;
        private System.Windows.Forms.Panel PnlSettings;
        private System.Windows.Forms.NumericUpDown NumSearchLimit;
        private System.Windows.Forms.Label LblSearchLimit;
        private System.Windows.Forms.Label LblGridSize;
        private System.Windows.Forms.TrackBar TBarGridSize;
        private System.Windows.Forms.CheckBox ChkHeavyDiagonals;
        private System.Windows.Forms.Label LblCompletedTimeValue;
        private System.Windows.Forms.Label LblCompletedTime;
        private System.Windows.Forms.CheckBox ChlShowProgress;
        private System.Windows.Forms.CheckBox ChkUseFastPathFinder;
        private System.Windows.Forms.CheckBox ChkReopenCloseNodes;
        private System.Windows.Forms.TrackBar TBarX;
        private System.Windows.Forms.TrackBar TBarY;

        private void BtnRecomputeMatrix_Click(object sender, EventArgs e)
        {
            float tryFloat;
            if (float.TryParse(MinZevel.Text, out tryFloat))
            {
                PnlGUI.ZLevel = tryFloat;
            }
            CollisionPlane CP = PnlGUI.CurrentPlane;
            if (CP == null) return;
            CP.NeedsUpdate = true;
            CP.EnsureUpToDate();
            PnlGUI.Invalidate();
        }

        private void BtnRebakeTerrain_Click(object sender, EventArgs e)
        {
            PnlGUI.CurrentPlane.NeedsUpdate = true;
            PathStore.BakeTerrain();
            PnlGUI.Invalidate();
        }

        private void MinZevel_TextChanged(object sender, EventArgs e)
        {
        }

        private delegate void CollisionPlaneListDelegate();
        public void CollisionPlaneListUpdate()
        {
            if (this.InvokeRequired)
            {
                Invoke(new CollisionPlaneListDelegate(CollisionPlaneListUpdate), new object[] { });
                return;
            }
           // lock (PathStore.Matrixes)
            try
            {
                foreach (CollisionPlane P in PathStore.Matrixes)
                {
                    if (!CollisionPlaneList.Items.Contains(P))
                    {
                        CollisionPlaneList.Items.Add(P);
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("CollisionPlaneListUpdate {0}", e);
            }
        }

        internal void OnNewCollisionPlane(CollisionPlane found)
        {
            PnlGUI.OnNewCollisionPlane(found);
            CollisionPlaneListUpdate();
        }

        private void CollisionPlaneList_SelectedIndexChanged(object sender, EventArgs e)
        {
            CollisionPlane o = (CollisionPlane)CollisionPlaneList.SelectedItem;
            PnlGUI.CurrentPlane = o;

        }
    }
}