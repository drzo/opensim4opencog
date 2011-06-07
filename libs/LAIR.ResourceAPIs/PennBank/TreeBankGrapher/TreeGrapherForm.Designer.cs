namespace TreeBankGrapher
{
    partial class TreeBankGrapher
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
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.treeDef = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.loadTreeBank = new System.Windows.Forms.Button();
            this.sentence = new System.Windows.Forms.ComboBox();
            this.mrgFile = new System.Windows.Forms.ComboBox();
            this.viewTree = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.createGraphFromDef = new System.Windows.Forms.Button();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.label3 = new System.Windows.Forms.Label();
            this.outputFormat = new System.Windows.Forms.ComboBox();
            this.menuStrip = new System.Windows.Forms.MenuStrip();
            this.grapherToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.optionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.resetMachineSpecificPathsMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.folderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.menuStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(19, 58);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(51, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "MRG file:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(14, 84);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(56, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Sentence:";
            // 
            // treeDef
            // 
            this.treeDef.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.treeDef.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.treeDef.Location = new System.Drawing.Point(6, 19);
            this.treeDef.Multiline = true;
            this.treeDef.Name = "treeDef";
            this.treeDef.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.treeDef.Size = new System.Drawing.Size(403, 414);
            this.treeDef.TabIndex = 5;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.loadTreeBank);
            this.groupBox1.Controls.Add(this.sentence);
            this.groupBox1.Controls.Add(this.mrgFile);
            this.groupBox1.Controls.Add(this.viewTree);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Location = new System.Drawing.Point(12, 31);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(241, 117);
            this.groupBox1.TabIndex = 6;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Specify a TreeBank sentence";
            // 
            // loadTreeBank
            // 
            this.loadTreeBank.Location = new System.Drawing.Point(17, 22);
            this.loadTreeBank.Name = "loadTreeBank";
            this.loadTreeBank.Size = new System.Drawing.Size(131, 23);
            this.loadTreeBank.TabIndex = 11;
            this.loadTreeBank.Text = "Load TreeBank";
            this.loadTreeBank.UseVisualStyleBackColor = true;
            this.loadTreeBank.Click += new System.EventHandler(this.loadTreeBank_Click);
            // 
            // sentence
            // 
            this.sentence.Enabled = false;
            this.sentence.FormattingEnabled = true;
            this.sentence.Location = new System.Drawing.Point(76, 80);
            this.sentence.Name = "sentence";
            this.sentence.Size = new System.Drawing.Size(72, 21);
            this.sentence.TabIndex = 10;
            // 
            // mrgFile
            // 
            this.mrgFile.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.mrgFile.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.mrgFile.DropDownHeight = 300;
            this.mrgFile.Enabled = false;
            this.mrgFile.FormattingEnabled = true;
            this.mrgFile.IntegralHeight = false;
            this.mrgFile.Location = new System.Drawing.Point(76, 55);
            this.mrgFile.Name = "mrgFile";
            this.mrgFile.Size = new System.Drawing.Size(72, 21);
            this.mrgFile.TabIndex = 9;
            this.mrgFile.SelectedIndexChanged += new System.EventHandler(this.mrgFile_SelectedIndexChanged);
            // 
            // viewTree
            // 
            this.viewTree.Enabled = false;
            this.viewTree.Location = new System.Drawing.Point(154, 78);
            this.viewTree.Name = "viewTree";
            this.viewTree.Size = new System.Drawing.Size(46, 23);
            this.viewTree.TabIndex = 8;
            this.viewTree.Text = "View";
            this.viewTree.UseVisualStyleBackColor = true;
            this.viewTree.Click += new System.EventHandler(this.viewTree_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox2.Controls.Add(this.createGraphFromDef);
            this.groupBox2.Controls.Add(this.treeDef);
            this.groupBox2.Location = new System.Drawing.Point(259, 31);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(415, 468);
            this.groupBox2.TabIndex = 7;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Or insert a tree definition";
            // 
            // createGraphFromDef
            // 
            this.createGraphFromDef.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.createGraphFromDef.Location = new System.Drawing.Point(330, 439);
            this.createGraphFromDef.Name = "createGraphFromDef";
            this.createGraphFromDef.Size = new System.Drawing.Size(79, 23);
            this.createGraphFromDef.TabIndex = 9;
            this.createGraphFromDef.Text = "Create graph";
            this.createGraphFromDef.UseVisualStyleBackColor = true;
            this.createGraphFromDef.Click += new System.EventHandler(this.createGraphFromDef_Click);
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.label3);
            this.groupBox3.Controls.Add(this.outputFormat);
            this.groupBox3.Location = new System.Drawing.Point(12, 154);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(241, 55);
            this.groupBox3.TabIndex = 8;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Options";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(19, 22);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(42, 13);
            this.label3.TabIndex = 1;
            this.label3.Text = "Output:";
            // 
            // outputFormat
            // 
            this.outputFormat.FormattingEnabled = true;
            this.outputFormat.Location = new System.Drawing.Point(66, 19);
            this.outputFormat.Name = "outputFormat";
            this.outputFormat.Size = new System.Drawing.Size(111, 21);
            this.outputFormat.TabIndex = 0;
            // 
            // menuStrip
            // 
            this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.grapherToolStripMenuItem,
            this.optionsToolStripMenuItem});
            this.menuStrip.Location = new System.Drawing.Point(0, 0);
            this.menuStrip.Name = "menuStrip";
            this.menuStrip.Size = new System.Drawing.Size(686, 24);
            this.menuStrip.TabIndex = 9;
            this.menuStrip.Text = "menuStrip1";
            // 
            // grapherToolStripMenuItem
            // 
            this.grapherToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.exitToolStripMenuItem});
            this.grapherToolStripMenuItem.Name = "grapherToolStripMenuItem";
            this.grapherToolStripMenuItem.Size = new System.Drawing.Size(58, 20);
            this.grapherToolStripMenuItem.Text = "Grapher";
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(103, 22);
            this.exitToolStripMenuItem.Text = "Exit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // optionsToolStripMenuItem
            // 
            this.optionsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.resetMachineSpecificPathsMenuItem});
            this.optionsToolStripMenuItem.Name = "optionsToolStripMenuItem";
            this.optionsToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
            this.optionsToolStripMenuItem.Text = "Options";
            // 
            // resetMachineSpecificPathsMenuItem
            // 
            this.resetMachineSpecificPathsMenuItem.Name = "resetMachineSpecificPathsMenuItem";
            this.resetMachineSpecificPathsMenuItem.Size = new System.Drawing.Size(224, 22);
            this.resetMachineSpecificPathsMenuItem.Text = "Reset machine-specific paths";
            this.resetMachineSpecificPathsMenuItem.Click += new System.EventHandler(this.resetMachineSpecificPathsMenuItem_Click);
            // 
            // TreeBankGrapher
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(686, 511);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.menuStrip);
            this.MainMenuStrip = this.menuStrip;
            this.Name = "TreeBankGrapher";
            this.Text = "TreeBank Grapher";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.menuStrip.ResumeLayout(false);
            this.menuStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox treeDef;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.Button viewTree;
        private System.Windows.Forms.Button createGraphFromDef;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox outputFormat;
        private System.Windows.Forms.MenuStrip menuStrip;
        private System.Windows.Forms.ToolStripMenuItem grapherToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.Button loadTreeBank;
        private System.Windows.Forms.ComboBox sentence;
        private System.Windows.Forms.ComboBox mrgFile;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog;
        private System.Windows.Forms.ToolStripMenuItem optionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem resetMachineSpecificPathsMenuItem;
    }
}

