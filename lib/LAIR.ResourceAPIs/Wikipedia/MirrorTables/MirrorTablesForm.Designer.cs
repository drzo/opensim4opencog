namespace MirrorTables
{
    partial class MirrorTablesForm
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
            this.startCreationBtn = new System.Windows.Forms.Button();
            this.titleIndex = new System.Windows.Forms.CheckBox();
            this.pageIndex = new System.Windows.Forms.CheckBox();
            this.statusBtn = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.stopCreationBtn = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.stopDumpBtn = new System.Windows.Forms.Button();
            this.getDumpedTitlesBtn = new System.Windows.Forms.Button();
            this.dumpBrowseBtn = new System.Windows.Forms.Button();
            this.dumpDirBox = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.dumpStatusBtn = new System.Windows.Forms.Button();
            this.startDumpBtn = new System.Windows.Forms.Button();
            this.startIDBox = new System.Windows.Forms.TextBox();
            this.numPagesBox = new System.Windows.Forms.TextBox();
            this.blockSizeBox = new System.Windows.Forms.TextBox();
            this.saveFile = new System.Windows.Forms.SaveFileDialog();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.dumpDirBrowse = new System.Windows.Forms.Button();
            this.b = new System.Windows.Forms.Label();
            this.lemurDumpTitleList = new System.Windows.Forms.TextBox();
            this.reportFileBrowseBtn = new System.Windows.Forms.Button();
            this.label11 = new System.Windows.Forms.Label();
            this.reportFileBox = new System.Windows.Forms.TextBox();
            this.lemurTitleListBrowse = new System.Windows.Forms.Button();
            this.lemurTitleListBox = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.startCheckBtn = new System.Windows.Forms.Button();
            this.folderBrowser = new System.Windows.Forms.FolderBrowserDialog();
            this.openFile = new System.Windows.Forms.OpenFileDialog();
            this.groupBox5 = new System.Windows.Forms.GroupBox();
            this.ignoreRedirectsChk = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox5.SuspendLayout();
            this.SuspendLayout();
            // 
            // startCreationBtn
            // 
            this.startCreationBtn.Location = new System.Drawing.Point(32, 74);
            this.startCreationBtn.Name = "startCreationBtn";
            this.startCreationBtn.Size = new System.Drawing.Size(108, 23);
            this.startCreationBtn.TabIndex = 4;
            this.startCreationBtn.Text = "Start Creation";
            this.startCreationBtn.UseVisualStyleBackColor = true;
            this.startCreationBtn.Click += new System.EventHandler(this.startCreationBtn_Click);
            // 
            // titleIndex
            // 
            this.titleIndex.AutoSize = true;
            this.titleIndex.Location = new System.Drawing.Point(37, 27);
            this.titleIndex.Name = "titleIndex";
            this.titleIndex.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.titleIndex.Size = new System.Drawing.Size(103, 17);
            this.titleIndex.TabIndex = 10;
            this.titleIndex.Text = "Write Title Index";
            this.titleIndex.UseVisualStyleBackColor = true;
            // 
            // pageIndex
            // 
            this.pageIndex.AutoSize = true;
            this.pageIndex.Location = new System.Drawing.Point(32, 51);
            this.pageIndex.Name = "pageIndex";
            this.pageIndex.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.pageIndex.Size = new System.Drawing.Size(108, 17);
            this.pageIndex.TabIndex = 11;
            this.pageIndex.Text = "Write Page Index";
            this.pageIndex.UseVisualStyleBackColor = true;
            // 
            // statusBtn
            // 
            this.statusBtn.Location = new System.Drawing.Point(32, 132);
            this.statusBtn.Name = "statusBtn";
            this.statusBtn.Size = new System.Drawing.Size(108, 23);
            this.statusBtn.TabIndex = 17;
            this.statusBtn.Text = "Get Status";
            this.statusBtn.UseVisualStyleBackColor = true;
            this.statusBtn.Click += new System.EventHandler(this.statusBtn_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.stopCreationBtn);
            this.groupBox1.Controls.Add(this.statusBtn);
            this.groupBox1.Controls.Add(this.startCreationBtn);
            this.groupBox1.Controls.Add(this.titleIndex);
            this.groupBox1.Controls.Add(this.pageIndex);
            this.groupBox1.Location = new System.Drawing.Point(263, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(159, 169);
            this.groupBox1.TabIndex = 18;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Mirror Table Creation";
            // 
            // stopCreationBtn
            // 
            this.stopCreationBtn.Enabled = false;
            this.stopCreationBtn.Location = new System.Drawing.Point(32, 103);
            this.stopCreationBtn.Name = "stopCreationBtn";
            this.stopCreationBtn.Size = new System.Drawing.Size(108, 23);
            this.stopCreationBtn.TabIndex = 18;
            this.stopCreationBtn.Text = "Stop Creation";
            this.stopCreationBtn.UseVisualStyleBackColor = true;
            this.stopCreationBtn.Click += new System.EventHandler(this.stopCreationBtn_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.stopDumpBtn);
            this.groupBox2.Controls.Add(this.getDumpedTitlesBtn);
            this.groupBox2.Controls.Add(this.dumpBrowseBtn);
            this.groupBox2.Controls.Add(this.dumpDirBox);
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Controls.Add(this.dumpStatusBtn);
            this.groupBox2.Controls.Add(this.startDumpBtn);
            this.groupBox2.Location = new System.Drawing.Point(12, 362);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(410, 96);
            this.groupBox2.TabIndex = 19;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Dump To Lemur";
            // 
            // stopDumpBtn
            // 
            this.stopDumpBtn.Enabled = false;
            this.stopDumpBtn.Location = new System.Drawing.Point(240, 58);
            this.stopDumpBtn.Name = "stopDumpBtn";
            this.stopDumpBtn.Size = new System.Drawing.Size(73, 23);
            this.stopDumpBtn.TabIndex = 32;
            this.stopDumpBtn.Text = "Stop Dump";
            this.stopDumpBtn.UseVisualStyleBackColor = true;
            this.stopDumpBtn.Click += new System.EventHandler(this.stopDumpBtn_Click);
            // 
            // getDumpedTitlesBtn
            // 
            this.getDumpedTitlesBtn.Location = new System.Drawing.Point(30, 57);
            this.getDumpedTitlesBtn.Name = "getDumpedTitlesBtn";
            this.getDumpedTitlesBtn.Size = new System.Drawing.Size(108, 23);
            this.getDumpedTitlesBtn.TabIndex = 31;
            this.getDumpedTitlesBtn.Text = "Get Dumped Titles";
            this.getDumpedTitlesBtn.UseVisualStyleBackColor = true;
            this.getDumpedTitlesBtn.Click += new System.EventHandler(this.getDumpedTitlesBtn_Click);
            // 
            // dumpBrowseBtn
            // 
            this.dumpBrowseBtn.Location = new System.Drawing.Point(318, 29);
            this.dumpBrowseBtn.Name = "dumpBrowseBtn";
            this.dumpBrowseBtn.Size = new System.Drawing.Size(73, 23);
            this.dumpBrowseBtn.TabIndex = 30;
            this.dumpBrowseBtn.Text = "Browse...";
            this.dumpBrowseBtn.UseVisualStyleBackColor = true;
            this.dumpBrowseBtn.Click += new System.EventHandler(this.dumpBrowseBtn_Click);
            // 
            // dumpDirBox
            // 
            this.dumpDirBox.Location = new System.Drawing.Point(71, 31);
            this.dumpDirBox.Name = "dumpDirBox";
            this.dumpDirBox.Size = new System.Drawing.Size(241, 20);
            this.dumpDirBox.TabIndex = 29;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(11, 34);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(54, 13);
            this.label8.TabIndex = 28;
            this.label8.Text = "Dump Dir:";
            // 
            // dumpStatusBtn
            // 
            this.dumpStatusBtn.Location = new System.Drawing.Point(144, 58);
            this.dumpStatusBtn.Name = "dumpStatusBtn";
            this.dumpStatusBtn.Size = new System.Drawing.Size(89, 23);
            this.dumpStatusBtn.TabIndex = 27;
            this.dumpStatusBtn.Text = "Get Status";
            this.dumpStatusBtn.UseVisualStyleBackColor = true;
            this.dumpStatusBtn.Click += new System.EventHandler(this.statusBtn_Click);
            // 
            // startDumpBtn
            // 
            this.startDumpBtn.Location = new System.Drawing.Point(318, 58);
            this.startDumpBtn.Name = "startDumpBtn";
            this.startDumpBtn.Size = new System.Drawing.Size(73, 23);
            this.startDumpBtn.TabIndex = 20;
            this.startDumpBtn.Text = "Start Dump";
            this.startDumpBtn.UseVisualStyleBackColor = true;
            this.startDumpBtn.Click += new System.EventHandler(this.startDumpBtn_Click);
            // 
            // startIDBox
            // 
            this.startIDBox.Location = new System.Drawing.Point(103, 25);
            this.startIDBox.Name = "startIDBox";
            this.startIDBox.Size = new System.Drawing.Size(100, 20);
            this.startIDBox.TabIndex = 24;
            this.startIDBox.Text = "0";
            // 
            // numPagesBox
            // 
            this.numPagesBox.Location = new System.Drawing.Point(103, 51);
            this.numPagesBox.Name = "numPagesBox";
            this.numPagesBox.Size = new System.Drawing.Size(100, 20);
            this.numPagesBox.TabIndex = 25;
            this.numPagesBox.Text = "10000";
            // 
            // blockSizeBox
            // 
            this.blockSizeBox.Location = new System.Drawing.Point(103, 77);
            this.blockSizeBox.Name = "blockSizeBox";
            this.blockSizeBox.Size = new System.Drawing.Size(100, 20);
            this.blockSizeBox.TabIndex = 26;
            this.blockSizeBox.Text = "5000";
            // 
            // saveFile
            // 
            this.saveFile.AddExtension = false;
            this.saveFile.Filter = "XML files | *.xml";
            this.saveFile.RestoreDirectory = true;
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.dumpDirBrowse);
            this.groupBox4.Controls.Add(this.b);
            this.groupBox4.Controls.Add(this.lemurDumpTitleList);
            this.groupBox4.Controls.Add(this.reportFileBrowseBtn);
            this.groupBox4.Controls.Add(this.label11);
            this.groupBox4.Controls.Add(this.reportFileBox);
            this.groupBox4.Controls.Add(this.lemurTitleListBrowse);
            this.groupBox4.Controls.Add(this.lemurTitleListBox);
            this.groupBox4.Controls.Add(this.label9);
            this.groupBox4.Controls.Add(this.startCheckBtn);
            this.groupBox4.Location = new System.Drawing.Point(12, 187);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(410, 169);
            this.groupBox4.TabIndex = 19;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Consistency Check";
            // 
            // dumpDirBrowse
            // 
            this.dumpDirBrowse.Location = new System.Drawing.Point(318, 38);
            this.dumpDirBrowse.Name = "dumpDirBrowse";
            this.dumpDirBrowse.Size = new System.Drawing.Size(73, 23);
            this.dumpDirBrowse.TabIndex = 44;
            this.dumpDirBrowse.Text = "Browse...";
            this.dumpDirBrowse.UseVisualStyleBackColor = true;
            this.dumpDirBrowse.Click += new System.EventHandler(this.dumpDirBrowse_Click);
            // 
            // b
            // 
            this.b.AutoSize = true;
            this.b.Location = new System.Drawing.Point(12, 43);
            this.b.Name = "b";
            this.b.Size = new System.Drawing.Size(112, 13);
            this.b.TabIndex = 42;
            this.b.Text = "Lemur Dump Title List:";
            // 
            // lemurDumpTitleList
            // 
            this.lemurDumpTitleList.Location = new System.Drawing.Point(129, 40);
            this.lemurDumpTitleList.Name = "lemurDumpTitleList";
            this.lemurDumpTitleList.Size = new System.Drawing.Size(184, 20);
            this.lemurDumpTitleList.TabIndex = 43;
            // 
            // reportFileBrowseBtn
            // 
            this.reportFileBrowseBtn.Location = new System.Drawing.Point(318, 90);
            this.reportFileBrowseBtn.Name = "reportFileBrowseBtn";
            this.reportFileBrowseBtn.Size = new System.Drawing.Size(73, 23);
            this.reportFileBrowseBtn.TabIndex = 35;
            this.reportFileBrowseBtn.Text = "Browse...";
            this.reportFileBrowseBtn.UseVisualStyleBackColor = true;
            this.reportFileBrowseBtn.Click += new System.EventHandler(this.reportFileBrowseBtn_Click);
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(63, 95);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(61, 13);
            this.label11.TabIndex = 34;
            this.label11.Text = "Report File:";
            // 
            // reportFileBox
            // 
            this.reportFileBox.Location = new System.Drawing.Point(130, 92);
            this.reportFileBox.Name = "reportFileBox";
            this.reportFileBox.Size = new System.Drawing.Size(183, 20);
            this.reportFileBox.TabIndex = 33;
            // 
            // lemurTitleListBrowse
            // 
            this.lemurTitleListBrowse.Location = new System.Drawing.Point(318, 64);
            this.lemurTitleListBrowse.Name = "lemurTitleListBrowse";
            this.lemurTitleListBrowse.Size = new System.Drawing.Size(73, 23);
            this.lemurTitleListBrowse.TabIndex = 31;
            this.lemurTitleListBrowse.Text = "Browse...";
            this.lemurTitleListBrowse.UseVisualStyleBackColor = true;
            this.lemurTitleListBrowse.Click += new System.EventHandler(this.lemurTitleListBrowse_Click);
            // 
            // lemurTitleListBox
            // 
            this.lemurTitleListBox.Location = new System.Drawing.Point(130, 66);
            this.lemurTitleListBox.Name = "lemurTitleListBox";
            this.lemurTitleListBox.Size = new System.Drawing.Size(183, 20);
            this.lemurTitleListBox.TabIndex = 12;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(14, 69);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(110, 13);
            this.label9.TabIndex = 0;
            this.label9.Text = "Lemur Index Title List:";
            // 
            // startCheckBtn
            // 
            this.startCheckBtn.Location = new System.Drawing.Point(238, 118);
            this.startCheckBtn.Name = "startCheckBtn";
            this.startCheckBtn.Size = new System.Drawing.Size(75, 23);
            this.startCheckBtn.TabIndex = 4;
            this.startCheckBtn.Text = "Start Check";
            this.startCheckBtn.UseVisualStyleBackColor = true;
            this.startCheckBtn.Click += new System.EventHandler(this.startCheckBtn_Click);
            // 
            // openFile
            // 
            this.openFile.RestoreDirectory = true;
            // 
            // groupBox5
            // 
            this.groupBox5.Controls.Add(this.ignoreRedirectsChk);
            this.groupBox5.Controls.Add(this.label3);
            this.groupBox5.Controls.Add(this.label2);
            this.groupBox5.Controls.Add(this.label1);
            this.groupBox5.Controls.Add(this.startIDBox);
            this.groupBox5.Controls.Add(this.numPagesBox);
            this.groupBox5.Controls.Add(this.blockSizeBox);
            this.groupBox5.Location = new System.Drawing.Point(12, 12);
            this.groupBox5.Name = "groupBox5";
            this.groupBox5.Size = new System.Drawing.Size(242, 169);
            this.groupBox5.TabIndex = 27;
            this.groupBox5.TabStop = false;
            this.groupBox5.Text = "Page Range";
            // 
            // ignoreRedirectsChk
            // 
            this.ignoreRedirectsChk.AutoSize = true;
            this.ignoreRedirectsChk.Location = new System.Drawing.Point(14, 103);
            this.ignoreRedirectsChk.Name = "ignoreRedirectsChk";
            this.ignoreRedirectsChk.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.ignoreRedirectsChk.Size = new System.Drawing.Size(104, 17);
            this.ignoreRedirectsChk.TabIndex = 30;
            this.ignoreRedirectsChk.Text = "Ignore Redirects";
            this.ignoreRedirectsChk.UseVisualStyleBackColor = true;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(35, 80);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(60, 13);
            this.label3.TabIndex = 29;
            this.label3.Text = "Block Size:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 54);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(92, 13);
            this.label2.TabIndex = 28;
            this.label2.Text = "Number of Pages:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(51, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(46, 13);
            this.label1.TabIndex = 27;
            this.label1.Text = "Start ID:";
            // 
            // MirrorTablesForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(436, 473);
            this.Controls.Add(this.groupBox5);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.MaximizeBox = false;
            this.Name = "MirrorTablesForm";
            this.Text = "Mirror Tables";
            this.Load += new System.EventHandler(this.MirrorTablesForm_Load);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.groupBox5.ResumeLayout(false);
            this.groupBox5.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button startCreationBtn;
        private System.Windows.Forms.CheckBox titleIndex;
        private System.Windows.Forms.CheckBox pageIndex;
        private System.Windows.Forms.Button statusBtn;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox startIDBox;
        private System.Windows.Forms.Button dumpStatusBtn;
        private System.Windows.Forms.TextBox numPagesBox;
        private System.Windows.Forms.Button startDumpBtn;
        private System.Windows.Forms.TextBox blockSizeBox;
        private System.Windows.Forms.TextBox dumpDirBox;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Button dumpBrowseBtn;
        private System.Windows.Forms.SaveFileDialog saveFile;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TextBox lemurTitleListBox;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Button startCheckBtn;
        private System.Windows.Forms.Button lemurTitleListBrowse;
        private System.Windows.Forms.Button reportFileBrowseBtn;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox reportFileBox;
        private System.Windows.Forms.FolderBrowserDialog folderBrowser;
        private System.Windows.Forms.OpenFileDialog openFile;
        private System.Windows.Forms.Button dumpDirBrowse;
        private System.Windows.Forms.Label b;
        private System.Windows.Forms.TextBox lemurDumpTitleList;
        private System.Windows.Forms.Button getDumpedTitlesBtn;
        private System.Windows.Forms.GroupBox groupBox5;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox ignoreRedirectsChk;
        private System.Windows.Forms.Button stopCreationBtn;
        private System.Windows.Forms.Button stopDumpBtn;
    }
}

