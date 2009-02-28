using cogbot.TheOpenSims.Navigation;
namespace cogbot.TheOpenSims.Navigation.Debug
{
    partial class PathFinderDemo
    {
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
            this.ChkReopenCloseNodes = new System.Windows.Forms.CheckBox();
            this.ChkUseFastPathFinder = new System.Windows.Forms.CheckBox();
            this.ChlShowProgress = new System.Windows.Forms.CheckBox();
            this.ChkHeavyDiagonals = new System.Windows.Forms.CheckBox();
            this.LblGridSize = new System.Windows.Forms.Label();
            this.NumSearchLimit = new System.Windows.Forms.NumericUpDown();
            this.TBarGridSize = new System.Windows.Forms.TrackBar();
            this.TBarX = new System.Windows.Forms.TrackBar();
            this.TBarY = new System.Windows.Forms.TrackBar();
            this.LblSearchLimit = new System.Windows.Forms.Label();
            this.LblCompletedTimeValue = new System.Windows.Forms.Label();
            this.LblCompletedTime = new System.Windows.Forms.Label();            
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
            this.BtnX,
            this.toolStripSeparator7,
            this.BtnStart,
            this.toolStripSeparator8,
            this.BtnEnd,
            this.toolStripSeparator9});
            this.ToolStrp.Location = new System.Drawing.Point(0, 537);
            this.ToolStrp.Name = "ToolStrp";
            this.ToolStrp.Size = new System.Drawing.Size(716, 25);
            this.ToolStrp.TabIndex = 10;
            this.ToolStrp.Text = "toolStrip1";
            // 
            // BtnNew
            // 
            this.BtnNew.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
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
            this.TBarSpeed.Location = new System.Drawing.Point(566, 24);
            this.TBarSpeed.Maximum = 60;
            this.TBarSpeed.Name = "TBarSpeed";
            this.TBarSpeed.Size = new System.Drawing.Size(121, 33);
            this.TBarSpeed.TabIndex = 11;
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
            this.LblSpeed.Location = new System.Drawing.Point(568, 9);
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
            this.ChkDiagonals.TabIndex = 13;
            this.ChkDiagonals.Text = "Diagonals";
            this.ChkDiagonals.UseVisualStyleBackColor = true;
            // 
            // ChkPunishChangeDirection
            // 
            this.ChkPunishChangeDirection.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkPunishChangeDirection.Location = new System.Drawing.Point(12, 47);
            this.ChkPunishChangeDirection.Name = "ChkPunishChangeDirection";
            this.ChkPunishChangeDirection.Size = new System.Drawing.Size(140, 30);
            this.ChkPunishChangeDirection.TabIndex = 14;
            this.ChkPunishChangeDirection.Text = "Punish Change Direction";
            this.ChkPunishChangeDirection.UseVisualStyleBackColor = true;
            // 
            // NumUpDownHeuristic
            // 
            this.NumUpDownHeuristic.Location = new System.Drawing.Point(12, 114);
            this.NumUpDownHeuristic.Name = "NumUpDownHeuristic";
            this.NumUpDownHeuristic.Size = new System.Drawing.Size(38, 20);
            this.NumUpDownHeuristic.TabIndex = 15;
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
            this.BtnPause.Location = new System.Drawing.Point(565, 504);
            this.BtnPause.Name = "BtnPause";
            this.BtnPause.Size = new System.Drawing.Size(143, 23);
            this.BtnPause.TabIndex = 17;
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
            this.CboFormula.TabIndex = 18;
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
            this.ChkTieBraker.TabIndex = 20;
            this.ChkTieBraker.Text = "Use Tie Breaker";
            this.ChkTieBraker.UseVisualStyleBackColor = true;
            // 
            // BtnStartStop
            // 
            this.BtnStartStop.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.BtnStartStop.Location = new System.Drawing.Point(565, 475);
            this.BtnStartStop.Name = "BtnStartStop";
            this.BtnStartStop.Size = new System.Drawing.Size(143, 23);
            this.BtnStartStop.TabIndex = 22;
            this.BtnStartStop.Text = "Run";
            this.BtnStartStop.UseVisualStyleBackColor = true;
            this.BtnStartStop.Click += new System.EventHandler(this.BtnStartStop_Click);
            // 
            // PnlSettings
            // 
            this.PnlSettings.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
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
            this.PnlSettings.Location = new System.Drawing.Point(561, 59);
            this.PnlSettings.Name = "PnlSettings";
            this.PnlSettings.Size = new System.Drawing.Size(155, 534);
            this.PnlSettings.TabIndex = 23;
            // 
            // ChkReopenCloseNodes
            // 
            this.ChkReopenCloseNodes.Checked = true;
            this.ChkReopenCloseNodes.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ChkReopenCloseNodes.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkReopenCloseNodes.Location = new System.Drawing.Point(12, 78);
            this.ChkReopenCloseNodes.Name = "ChkReopenCloseNodes";
            this.ChkReopenCloseNodes.Size = new System.Drawing.Size(140, 20);
            this.ChkReopenCloseNodes.TabIndex = 31;
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
            this.ChkUseFastPathFinder.TabIndex = 30;
            this.ChkUseFastPathFinder.Text = "Fast PathFinder";
            this.ChkUseFastPathFinder.UseVisualStyleBackColor = true;
            // 
            // ChlShowProgress
            // 
            this.ChlShowProgress.AutoSize = true;
            this.ChlShowProgress.Checked = true;
            this.ChlShowProgress.CheckState = System.Windows.Forms.CheckState.Checked;
            this.ChlShowProgress.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChlShowProgress.Location = new System.Drawing.Point(12, 311);
            this.ChlShowProgress.Name = "ChlShowProgress";
            this.ChlShowProgress.Size = new System.Drawing.Size(97, 17);
            this.ChlShowProgress.TabIndex = 29;
            this.ChlShowProgress.Text = "Show Progress";
            this.ChlShowProgress.UseVisualStyleBackColor = true;
            // 
            // ChkHeavyDiagonals
            // 
            this.ChkHeavyDiagonals.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F);
            this.ChkHeavyDiagonals.Location = new System.Drawing.Point(12, 24);
            this.ChkHeavyDiagonals.Name = "ChkHeavyDiagonals";
            this.ChkHeavyDiagonals.Size = new System.Drawing.Size(140, 20);
            this.ChkHeavyDiagonals.TabIndex = 26;
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
            this.NumSearchLimit.TabIndex = 22;
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
            this.TBarGridSize.Location = new System.Drawing.Point(5, 249);
            this.TBarGridSize.Maximum = 20;
            this.TBarGridSize.Minimum = 1;
            this.TBarGridSize.Name = "TBarGridSize";
            this.TBarGridSize.Size = new System.Drawing.Size(144, 33);
            this.TBarGridSize.TabIndex = 24;
            this.TBarGridSize.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarGridSize.Value = 1;
            this.TBarGridSize.Scroll += new System.EventHandler(this.TBarGridSize_Scroll);
            // 
            // TBarX
            // 
            this.TBarX.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.TBarX.AutoSize = false;
            this.TBarX.Location = new System.Drawing.Point(5, 353);
            this.TBarX.Maximum = 254;
            this.TBarX.Minimum = 0;
            this.TBarX.Name = "TBarX";
            this.TBarX.Size = new System.Drawing.Size(144, 33);
            this.TBarX.TabIndex = 32;
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
            this.TBarY.Minimum = 0;
            this.TBarY.Name = "TBarY";
            this.TBarY.Size = new System.Drawing.Size(144, 33);
            this.TBarY.TabIndex = 24;
            this.TBarY.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.TBarY.Value = 1;
            this.TBarY.Scroll += new System.EventHandler(this.TBarXY_Scroll);
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
            // LblCompletedTimeValue
            // 
            this.LblCompletedTimeValue.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.LblCompletedTimeValue.Location = new System.Drawing.Point(644, 396);
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
            this.LblCompletedTime.Location = new System.Drawing.Point(564, 396);
            this.LblCompletedTime.Name = "LblCompletedTime";
            this.LblCompletedTime.Size = new System.Drawing.Size(148, 13);
            this.LblCompletedTime.TabIndex = 27;
            this.LblCompletedTime.Text = "Completed Time               sec.";
            // 
            // PnlGUI
            // 
            this.PnlGUI.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.PnlGUI.BackColor = System.Drawing.Color.White;
            this.PnlGUI.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.PnlGUI.DrawModeSetup = cogbot.TheOpenSims.Navigation.Debug.DrawModeSetup.None;
            this.PnlGUI.End = new System.Drawing.Point(0, 0);
            this.PnlGUI.Formula = cogbot.TheOpenSims.Navigation.HeuristicFormula.Manhattan;
            this.PnlGUI.GridSize = 1;
            this.PnlGUI.Location = new System.Drawing.Point(-1, -1);
            this.PnlGUI.Name = "PnlGUI";
            this.PnlGUI.NodeWeight = ((byte)(1));
            this.PnlGUI.Size = new System.Drawing.Size(556, 528);
            this.PnlGUI.Start = new System.Drawing.Point(1, 1);
            this.PnlGUI.TabIndex = 21;
            // 
            // PathFinderDemo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(716, 562);
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
        private System.Windows.Forms.CheckBox ChkTieBraker;
        public PanelPathFinder PnlGUI;
        private System.Windows.Forms.Button BtnStartStop;
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
    }
}

