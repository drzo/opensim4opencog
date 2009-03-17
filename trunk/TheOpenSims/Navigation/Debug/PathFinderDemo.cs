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
using cogbot.TheOpenSims.Navigation;
namespace cogbot.TheOpenSims.Navigation.Debug
{
    [Author("Franco, Gustavo")]
    public partial class PathFinderDemo : Form
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
            PnlGUI.PathStore = pathStore;

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
                String name = S.GetSimRegion().RegionName + " Level " + S.SimLevel(128, 128);
                this.Text = name;
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

                    mPathFinder = new PathFinderFasting(PnlGUI.PathStore.mMatrix);
                    mPathFinder.PathFinderDebug += new PathFinderDebugHandler(PathFinderDebug);
                }
            }
            else
            {
                if (!(mPathFinder is PathFinder))
                {
                    if (mPathFinder != null)
                        mPathFinder.PathFinderDebug -= new PathFinderDebugHandler(PathFinderDebug);

                    mPathFinder = new PathFinder(PnlGUI.Matrix);
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

        internal void ShowPath(List<PathFinderNode> pfn)
        {
            PnlGUI.DrawPath(pfn);
        }
    }
}