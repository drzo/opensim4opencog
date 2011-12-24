namespace SemLinkEditor
{
    partial class SemLinkEditorForm
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
            this.semLinkBrowse = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.semLinkDirectory = new System.Windows.Forms.TextBox();
            this.verbNetBrowse = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.verbNetDirectory = new System.Windows.Forms.TextBox();
            this.folderBrowser = new System.Windows.Forms.FolderBrowserDialog();
            this.createIssueFile = new System.Windows.Forms.Button();
            this.output1 = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.propBankArguments = new System.Windows.Forms.ListBox();
            this.label5 = new System.Windows.Forms.Label();
            this.propBankRoleSets = new System.Windows.Forms.ListBox();
            this.label3 = new System.Windows.Forms.Label();
            this.propBankVerb = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.showClasses = new System.Windows.Forms.Button();
            this.showClassesFor = new System.Windows.Forms.TextBox();
            this.link = new System.Windows.Forms.Button();
            this.unlink = new System.Windows.Forms.Button();
            this.verbNetRoles = new System.Windows.Forms.ListBox();
            this.label7 = new System.Windows.Forms.Label();
            this.verbNetClasses = new System.Windows.Forms.ListBox();
            this.label6 = new System.Windows.Forms.Label();
            this.currentMapping = new System.Windows.Forms.ListBox();
            this.label8 = new System.Windows.Forms.Label();
            this.output2 = new System.Windows.Forms.TextBox();
            this.mainMenu = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openIssueFile = new System.Windows.Forms.ToolStripMenuItem();
            this.save = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.exit = new System.Windows.Forms.ToolStripMenuItem();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.lexicalUnits = new System.Windows.Forms.ListBox();
            this.frameElements = new System.Windows.Forms.ListBox();
            this.label10 = new System.Windows.Forms.Label();
            this.frames = new System.Windows.Forms.ComboBox();
            this.label11 = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.mainMenu.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // semLinkBrowse
            // 
            this.semLinkBrowse.Location = new System.Drawing.Point(541, 33);
            this.semLinkBrowse.Name = "semLinkBrowse";
            this.semLinkBrowse.Size = new System.Drawing.Size(120, 23);
            this.semLinkBrowse.TabIndex = 2;
            this.semLinkBrowse.Text = "Browse...";
            this.semLinkBrowse.UseVisualStyleBackColor = true;
            this.semLinkBrowse.Click += new System.EventHandler(this.semLinkBrowse_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(20, 38);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(94, 13);
            this.label4.TabIndex = 11;
            this.label4.Text = "SemLink directory:";
            // 
            // semLinkDirectory
            // 
            this.semLinkDirectory.Location = new System.Drawing.Point(118, 35);
            this.semLinkDirectory.Name = "semLinkDirectory";
            this.semLinkDirectory.ReadOnly = true;
            this.semLinkDirectory.Size = new System.Drawing.Size(417, 20);
            this.semLinkDirectory.TabIndex = 6;
            this.semLinkDirectory.TextChanged += new System.EventHandler(this.semLinkDirectory_TextChanged);
            // 
            // verbNetBrowse
            // 
            this.verbNetBrowse.Location = new System.Drawing.Point(541, 59);
            this.verbNetBrowse.Name = "verbNetBrowse";
            this.verbNetBrowse.Size = new System.Drawing.Size(120, 23);
            this.verbNetBrowse.TabIndex = 3;
            this.verbNetBrowse.Text = "Browse...";
            this.verbNetBrowse.UseVisualStyleBackColor = true;
            this.verbNetBrowse.Click += new System.EventHandler(this.verbNetBrowse_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(20, 64);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(92, 13);
            this.label1.TabIndex = 14;
            this.label1.Text = "VerbNet directory:";
            // 
            // verbNetDirectory
            // 
            this.verbNetDirectory.Location = new System.Drawing.Point(118, 61);
            this.verbNetDirectory.Name = "verbNetDirectory";
            this.verbNetDirectory.ReadOnly = true;
            this.verbNetDirectory.Size = new System.Drawing.Size(417, 20);
            this.verbNetDirectory.TabIndex = 7;
            this.verbNetDirectory.TextChanged += new System.EventHandler(this.verbNetDirectory_TextChanged);
            // 
            // createIssueFile
            // 
            this.createIssueFile.Location = new System.Drawing.Point(667, 59);
            this.createIssueFile.Name = "createIssueFile";
            this.createIssueFile.Size = new System.Drawing.Size(120, 23);
            this.createIssueFile.TabIndex = 4;
            this.createIssueFile.Text = "Create issue file...";
            this.createIssueFile.UseVisualStyleBackColor = true;
            this.createIssueFile.Click += new System.EventHandler(this.createIssueFile_Click);
            // 
            // output1
            // 
            this.output1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output1.Location = new System.Drawing.Point(12, 586);
            this.output1.Multiline = true;
            this.output1.Name = "output1";
            this.output1.ReadOnly = true;
            this.output1.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output1.Size = new System.Drawing.Size(399, 114);
            this.output1.TabIndex = 2;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.propBankArguments);
            this.groupBox1.Controls.Add(this.label5);
            this.groupBox1.Controls.Add(this.propBankRoleSets);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.propBankVerb);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Location = new System.Drawing.Point(12, 100);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(809, 146);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "PropBank";
            // 
            // propBankArguments
            // 
            this.propBankArguments.FormattingEnabled = true;
            this.propBankArguments.HorizontalScrollbar = true;
            this.propBankArguments.Location = new System.Drawing.Point(542, 36);
            this.propBankArguments.Name = "propBankArguments";
            this.propBankArguments.Size = new System.Drawing.Size(255, 95);
            this.propBankArguments.Sorted = true;
            this.propBankArguments.TabIndex = 2;
            this.propBankArguments.SelectedIndexChanged += new System.EventHandler(this.propBankArguments_SelectedIndexChanged);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(539, 20);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(60, 13);
            this.label5.TabIndex = 5;
            this.label5.Text = "Arguments:";
            // 
            // propBankRoleSets
            // 
            this.propBankRoleSets.FormattingEnabled = true;
            this.propBankRoleSets.HorizontalScrollbar = true;
            this.propBankRoleSets.Location = new System.Drawing.Point(281, 36);
            this.propBankRoleSets.Name = "propBankRoleSets";
            this.propBankRoleSets.Size = new System.Drawing.Size(255, 95);
            this.propBankRoleSets.Sorted = true;
            this.propBankRoleSets.TabIndex = 1;
            this.propBankRoleSets.SelectedIndexChanged += new System.EventHandler(this.propBankRoleSets_SelectedIndexChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(278, 20);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(54, 13);
            this.label3.TabIndex = 3;
            this.label3.Text = "Role sets:";
            // 
            // propBankVerb
            // 
            this.propBankVerb.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.propBankVerb.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.propBankVerb.DropDownHeight = 200;
            this.propBankVerb.FormattingEnabled = true;
            this.propBankVerb.IntegralHeight = false;
            this.propBankVerb.Location = new System.Drawing.Point(20, 36);
            this.propBankVerb.Name = "propBankVerb";
            this.propBankVerb.Size = new System.Drawing.Size(255, 21);
            this.propBankVerb.Sorted = true;
            this.propBankVerb.TabIndex = 0;
            this.propBankVerb.SelectedIndexChanged += new System.EventHandler(this.propBankVerb_SelectedIndexChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(17, 20);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(32, 13);
            this.label2.TabIndex = 1;
            this.label2.Text = "Verb:";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.showClasses);
            this.groupBox2.Controls.Add(this.showClassesFor);
            this.groupBox2.Controls.Add(this.link);
            this.groupBox2.Controls.Add(this.unlink);
            this.groupBox2.Controls.Add(this.verbNetRoles);
            this.groupBox2.Controls.Add(this.label7);
            this.groupBox2.Controls.Add(this.verbNetClasses);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.currentMapping);
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Location = new System.Drawing.Point(12, 252);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(809, 176);
            this.groupBox2.TabIndex = 1;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "VerbNet";
            // 
            // showClasses
            // 
            this.showClasses.Location = new System.Drawing.Point(179, 142);
            this.showClasses.Name = "showClasses";
            this.showClasses.Size = new System.Drawing.Size(96, 23);
            this.showClasses.TabIndex = 2;
            this.showClasses.Text = "Show classes";
            this.showClasses.UseVisualStyleBackColor = true;
            this.showClasses.Click += new System.EventHandler(this.showClasses_Click);
            // 
            // showClassesFor
            // 
            this.showClassesFor.Location = new System.Drawing.Point(20, 144);
            this.showClassesFor.Name = "showClassesFor";
            this.showClassesFor.Size = new System.Drawing.Size(153, 20);
            this.showClassesFor.TabIndex = 1;
            this.showClassesFor.KeyDown += new System.Windows.Forms.KeyEventHandler(this.classesForVerb_KeyDown);
            // 
            // link
            // 
            this.link.Location = new System.Drawing.Point(489, 142);
            this.link.Name = "link";
            this.link.Size = new System.Drawing.Size(47, 23);
            this.link.TabIndex = 4;
            this.link.Text = "Link";
            this.link.UseVisualStyleBackColor = true;
            this.link.Click += new System.EventHandler(this.link_Click);
            // 
            // unlink
            // 
            this.unlink.Location = new System.Drawing.Point(747, 142);
            this.unlink.Name = "unlink";
            this.unlink.Size = new System.Drawing.Size(47, 23);
            this.unlink.TabIndex = 6;
            this.unlink.Text = "Unlink";
            this.unlink.UseVisualStyleBackColor = true;
            this.unlink.Click += new System.EventHandler(this.unlink_Click);
            // 
            // verbNetRoles
            // 
            this.verbNetRoles.FormattingEnabled = true;
            this.verbNetRoles.Location = new System.Drawing.Point(281, 41);
            this.verbNetRoles.Name = "verbNetRoles";
            this.verbNetRoles.Size = new System.Drawing.Size(255, 95);
            this.verbNetRoles.Sorted = true;
            this.verbNetRoles.TabIndex = 3;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(278, 25);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(78, 13);
            this.label7.TabIndex = 9;
            this.label7.Text = "Available roles:";
            // 
            // verbNetClasses
            // 
            this.verbNetClasses.FormattingEnabled = true;
            this.verbNetClasses.HorizontalScrollbar = true;
            this.verbNetClasses.Location = new System.Drawing.Point(20, 41);
            this.verbNetClasses.Name = "verbNetClasses";
            this.verbNetClasses.Size = new System.Drawing.Size(255, 95);
            this.verbNetClasses.Sorted = true;
            this.verbNetClasses.TabIndex = 0;
            this.verbNetClasses.SelectedIndexChanged += new System.EventHandler(this.verbNetClasses_SelectedIndexChanged);
            this.verbNetClasses.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.verbNetClasses_MouseDoubleClick);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(17, 25);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(91, 13);
            this.label6.TabIndex = 7;
            this.label6.Text = "Available classes:";
            // 
            // currentMapping
            // 
            this.currentMapping.FormattingEnabled = true;
            this.currentMapping.HorizontalScrollbar = true;
            this.currentMapping.Location = new System.Drawing.Point(542, 41);
            this.currentMapping.Name = "currentMapping";
            this.currentMapping.Size = new System.Drawing.Size(252, 95);
            this.currentMapping.Sorted = true;
            this.currentMapping.TabIndex = 5;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(539, 25);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(87, 13);
            this.label8.TabIndex = 11;
            this.label8.Text = "Current mapping:";
            // 
            // output2
            // 
            this.output2.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output2.Location = new System.Drawing.Point(422, 586);
            this.output2.Multiline = true;
            this.output2.Name = "output2";
            this.output2.ReadOnly = true;
            this.output2.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output2.Size = new System.Drawing.Size(399, 114);
            this.output2.TabIndex = 15;
            // 
            // mainMenu
            // 
            this.mainMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem});
            this.mainMenu.Location = new System.Drawing.Point(0, 0);
            this.mainMenu.Name = "mainMenu";
            this.mainMenu.Size = new System.Drawing.Size(836, 24);
            this.mainMenu.TabIndex = 16;
            this.mainMenu.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openIssueFile,
            this.save,
            this.toolStripSeparator1,
            this.exit});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // openIssueFile
            // 
            this.openIssueFile.Name = "openIssueFile";
            this.openIssueFile.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.openIssueFile.Size = new System.Drawing.Size(207, 22);
            this.openIssueFile.Text = "Open issue file...";
            this.openIssueFile.Click += new System.EventHandler(this.openIssueFile_Click);
            // 
            // save
            // 
            this.save.Enabled = false;
            this.save.Name = "save";
            this.save.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.S)));
            this.save.Size = new System.Drawing.Size(207, 22);
            this.save.Text = "Save";
            this.save.Click += new System.EventHandler(this.save_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(204, 6);
            // 
            // exit
            // 
            this.exit.Name = "exit";
            this.exit.Size = new System.Drawing.Size(207, 22);
            this.exit.Text = "Exit";
            this.exit.Click += new System.EventHandler(this.exit_Click);
            // 
            // saveFileDialog
            // 
            this.saveFileDialog.DefaultExt = "txt";
            // 
            // openFileDialog
            // 
            this.openFileDialog.DefaultExt = "txt";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.lexicalUnits);
            this.groupBox3.Controls.Add(this.frameElements);
            this.groupBox3.Controls.Add(this.label10);
            this.groupBox3.Controls.Add(this.frames);
            this.groupBox3.Controls.Add(this.label11);
            this.groupBox3.Location = new System.Drawing.Point(12, 434);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(809, 146);
            this.groupBox3.TabIndex = 17;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "FrameNet";
            // 
            // lexicalUnits
            // 
            this.lexicalUnits.FormattingEnabled = true;
            this.lexicalUnits.HorizontalScrollbar = true;
            this.lexicalUnits.Location = new System.Drawing.Point(20, 63);
            this.lexicalUnits.Name = "lexicalUnits";
            this.lexicalUnits.Size = new System.Drawing.Size(255, 69);
            this.lexicalUnits.Sorted = true;
            this.lexicalUnits.TabIndex = 4;
            this.lexicalUnits.SelectedIndexChanged += new System.EventHandler(this.lexicalUnits_SelectedIndexChanged);
            // 
            // frameElements
            // 
            this.frameElements.FormattingEnabled = true;
            this.frameElements.HorizontalScrollbar = true;
            this.frameElements.Location = new System.Drawing.Point(281, 36);
            this.frameElements.Name = "frameElements";
            this.frameElements.Size = new System.Drawing.Size(513, 95);
            this.frameElements.Sorted = true;
            this.frameElements.TabIndex = 1;
            this.frameElements.SelectedIndexChanged += new System.EventHandler(this.frameElements_SelectedIndexChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(278, 20);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(84, 13);
            this.label10.TabIndex = 3;
            this.label10.Text = "Frame elements:";
            // 
            // frames
            // 
            this.frames.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.frames.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.frames.DropDownHeight = 200;
            this.frames.FormattingEnabled = true;
            this.frames.IntegralHeight = false;
            this.frames.Location = new System.Drawing.Point(20, 36);
            this.frames.Name = "frames";
            this.frames.Size = new System.Drawing.Size(255, 21);
            this.frames.Sorted = true;
            this.frames.TabIndex = 0;
            this.frames.SelectedIndexChanged += new System.EventHandler(this.frames_SelectedIndexChanged);
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(17, 20);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(39, 13);
            this.label11.TabIndex = 1;
            this.label11.Text = "Frame:";
            // 
            // SemLinkEditorForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(836, 712);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.output2);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.output1);
            this.Controls.Add(this.createIssueFile);
            this.Controls.Add(this.verbNetBrowse);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.verbNetDirectory);
            this.Controls.Add(this.semLinkBrowse);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.semLinkDirectory);
            this.Controls.Add(this.mainMenu);
            this.KeyPreview = true;
            this.MainMenuStrip = this.mainMenu;
            this.Name = "SemLinkEditorForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "SemLink Editor";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.SemLinkEditorForm_FormClosing);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.SemLinkEditorForm_KeyDown);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.mainMenu.ResumeLayout(false);
            this.mainMenu.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button semLinkBrowse;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox semLinkDirectory;
        private System.Windows.Forms.Button verbNetBrowse;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox verbNetDirectory;
        private System.Windows.Forms.FolderBrowserDialog folderBrowser;
        private System.Windows.Forms.Button createIssueFile;
        private System.Windows.Forms.TextBox output1;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.ListBox propBankArguments;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ListBox propBankRoleSets;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox propBankVerb;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.ListBox verbNetClasses;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ListBox verbNetRoles;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button link;
        private System.Windows.Forms.Button unlink;
        private System.Windows.Forms.ListBox currentMapping;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox output2;
        private System.Windows.Forms.MenuStrip mainMenu;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openIssueFile;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.ToolStripMenuItem save;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem exit;
        private System.Windows.Forms.Button showClasses;
        private System.Windows.Forms.TextBox showClassesFor;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.ListBox frameElements;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.ComboBox frames;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.ListBox lexicalUnits;
    }
}

