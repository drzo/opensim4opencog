namespace SemLinkEditor
{
    partial class IssueForm
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
            this.pbVerbsInVnNotSl = new System.Windows.Forms.CheckBox();
            this.issueFileBrowse = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.issueFilePath = new System.Windows.Forms.TextBox();
            this.create = new System.Windows.Forms.Button();
            this.pbVerbsInvalidVnClass = new System.Windows.Forms.CheckBox();
            this.invalidVnRole = new System.Windows.Forms.CheckBox();
            this.pbVerbsNotInVN = new System.Windows.Forms.CheckBox();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.invalidFrameElements = new System.Windows.Forms.CheckBox();
            this.invalidPropBankRoles = new System.Windows.Forms.CheckBox();
            this.fnFramesInvalidVnClass = new System.Windows.Forms.CheckBox();
            this.propBankReVerbs = new System.Windows.Forms.CheckBox();
            this.includeFN = new System.Windows.Forms.CheckBox();
            this.includePB = new System.Windows.Forms.CheckBox();
            this.cancel = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // pbVerbsInVnNotSl
            // 
            this.pbVerbsInVnNotSl.AutoSize = true;
            this.pbVerbsInVnNotSl.Location = new System.Drawing.Point(16, 27);
            this.pbVerbsInVnNotSl.Name = "pbVerbsInVnNotSl";
            this.pbVerbsInVnNotSl.Size = new System.Drawing.Size(246, 17);
            this.pbVerbsInVnNotSl.TabIndex = 0;
            this.pbVerbsInVnNotSl.Text = "PropBank verbs in VerbNet but not in SemLink";
            this.pbVerbsInVnNotSl.UseVisualStyleBackColor = true;
            // 
            // issueFileBrowse
            // 
            this.issueFileBrowse.Location = new System.Drawing.Point(507, 366);
            this.issueFileBrowse.Name = "issueFileBrowse";
            this.issueFileBrowse.Size = new System.Drawing.Size(120, 23);
            this.issueFileBrowse.TabIndex = 3;
            this.issueFileBrowse.Text = "Browse...";
            this.issueFileBrowse.UseVisualStyleBackColor = true;
            this.issueFileBrowse.Click += new System.EventHandler(this.issueFileBrowse_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(12, 371);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(66, 13);
            this.label4.TabIndex = 14;
            this.label4.Text = "Output path:";
            // 
            // issueFilePath
            // 
            this.issueFilePath.Location = new System.Drawing.Point(84, 368);
            this.issueFilePath.Name = "issueFilePath";
            this.issueFilePath.ReadOnly = true;
            this.issueFilePath.Size = new System.Drawing.Size(417, 20);
            this.issueFilePath.TabIndex = 2;
            this.issueFilePath.TextChanged += new System.EventHandler(this.issueFilePath_TextChanged);
            // 
            // create
            // 
            this.create.Enabled = false;
            this.create.Location = new System.Drawing.Point(322, 400);
            this.create.Name = "create";
            this.create.Size = new System.Drawing.Size(120, 23);
            this.create.TabIndex = 5;
            this.create.Text = "Create";
            this.create.UseVisualStyleBackColor = true;
            this.create.Click += new System.EventHandler(this.create_Click);
            // 
            // pbVerbsInvalidVnClass
            // 
            this.pbVerbsInvalidVnClass.AutoSize = true;
            this.pbVerbsInvalidVnClass.Location = new System.Drawing.Point(16, 50);
            this.pbVerbsInvalidVnClass.Name = "pbVerbsInvalidVnClass";
            this.pbVerbsInvalidVnClass.Size = new System.Drawing.Size(369, 17);
            this.pbVerbsInvalidVnClass.TabIndex = 1;
            this.pbVerbsInvalidVnClass.Text = "PropBank verbs mapped to non-existent or non-containing VerbNet class";
            this.pbVerbsInvalidVnClass.UseVisualStyleBackColor = true;
            // 
            // invalidVnRole
            // 
            this.invalidVnRole.AutoSize = true;
            this.invalidVnRole.Location = new System.Drawing.Point(16, 142);
            this.invalidVnRole.Name = "invalidVnRole";
            this.invalidVnRole.Size = new System.Drawing.Size(167, 17);
            this.invalidVnRole.TabIndex = 3;
            this.invalidVnRole.Text = "Invalid VerbNet thematic roles";
            this.invalidVnRole.UseVisualStyleBackColor = true;
            this.invalidVnRole.CheckedChanged += new System.EventHandler(this.invalidVnRole_CheckedChanged);
            // 
            // pbVerbsNotInVN
            // 
            this.pbVerbsNotInVN.AutoSize = true;
            this.pbVerbsNotInVN.Location = new System.Drawing.Point(16, 73);
            this.pbVerbsNotInVN.Name = "pbVerbsNotInVN";
            this.pbVerbsNotInVN.Size = new System.Drawing.Size(173, 17);
            this.pbVerbsNotInVN.TabIndex = 2;
            this.pbVerbsNotInVN.Text = "PropBank verbs not in VerbNet";
            this.pbVerbsNotInVN.UseVisualStyleBackColor = true;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.invalidFrameElements);
            this.groupBox1.Controls.Add(this.invalidPropBankRoles);
            this.groupBox1.Controls.Add(this.fnFramesInvalidVnClass);
            this.groupBox1.Controls.Add(this.propBankReVerbs);
            this.groupBox1.Controls.Add(this.includeFN);
            this.groupBox1.Controls.Add(this.includePB);
            this.groupBox1.Controls.Add(this.pbVerbsInVnNotSl);
            this.groupBox1.Controls.Add(this.pbVerbsNotInVN);
            this.groupBox1.Controls.Add(this.pbVerbsInvalidVnClass);
            this.groupBox1.Controls.Add(this.invalidVnRole);
            this.groupBox1.Location = new System.Drawing.Point(84, 18);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(417, 254);
            this.groupBox1.TabIndex = 1;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Issues";
            // 
            // invalidFrameElements
            // 
            this.invalidFrameElements.AutoSize = true;
            this.invalidFrameElements.Location = new System.Drawing.Point(16, 188);
            this.invalidFrameElements.Name = "invalidFrameElements";
            this.invalidFrameElements.Size = new System.Drawing.Size(247, 17);
            this.invalidFrameElements.TabIndex = 9;
            this.invalidFrameElements.Text = "Invalid frame elements (manual inspection only)";
            this.invalidFrameElements.UseVisualStyleBackColor = true;
            // 
            // invalidPropBankRoles
            // 
            this.invalidPropBankRoles.AutoSize = true;
            this.invalidPropBankRoles.Location = new System.Drawing.Point(16, 119);
            this.invalidPropBankRoles.Name = "invalidPropBankRoles";
            this.invalidPropBankRoles.Size = new System.Drawing.Size(248, 17);
            this.invalidPropBankRoles.TabIndex = 8;
            this.invalidPropBankRoles.Text = "Invalid PropBank roles (manual inspection only)";
            this.invalidPropBankRoles.UseVisualStyleBackColor = true;
            // 
            // fnFramesInvalidVnClass
            // 
            this.fnFramesInvalidVnClass.AutoSize = true;
            this.fnFramesInvalidVnClass.Location = new System.Drawing.Point(16, 165);
            this.fnFramesInvalidVnClass.Name = "fnFramesInvalidVnClass";
            this.fnFramesInvalidVnClass.Size = new System.Drawing.Size(373, 17);
            this.fnFramesInvalidVnClass.TabIndex = 7;
            this.fnFramesInvalidVnClass.Text = "FrameNet frames mapped to non-existent or non-containing VerbNet class";
            this.fnFramesInvalidVnClass.UseVisualStyleBackColor = true;
            // 
            // propBankReVerbs
            // 
            this.propBankReVerbs.AutoSize = true;
            this.propBankReVerbs.Location = new System.Drawing.Point(16, 96);
            this.propBankReVerbs.Name = "propBankReVerbs";
            this.propBankReVerbs.Size = new System.Drawing.Size(167, 17);
            this.propBankReVerbs.TabIndex = 6;
            this.propBankReVerbs.Text = "PropBank verbs with re- prefix";
            this.propBankReVerbs.UseVisualStyleBackColor = true;
            // 
            // includeFN
            // 
            this.includeFN.AutoSize = true;
            this.includeFN.Enabled = false;
            this.includeFN.Location = new System.Drawing.Point(306, 142);
            this.includeFN.Name = "includeFN";
            this.includeFN.Size = new System.Drawing.Size(110, 17);
            this.includeFN.TabIndex = 5;
            this.includeFN.Text = "Include FrameNet";
            this.includeFN.UseVisualStyleBackColor = true;
            // 
            // includePB
            // 
            this.includePB.AutoSize = true;
            this.includePB.Enabled = false;
            this.includePB.Location = new System.Drawing.Point(189, 142);
            this.includePB.Name = "includePB";
            this.includePB.Size = new System.Drawing.Size(111, 17);
            this.includePB.TabIndex = 4;
            this.includePB.Text = "Include PropBank";
            this.includePB.UseVisualStyleBackColor = true;
            // 
            // cancel
            // 
            this.cancel.Location = new System.Drawing.Point(196, 400);
            this.cancel.Name = "cancel";
            this.cancel.Size = new System.Drawing.Size(120, 23);
            this.cancel.TabIndex = 4;
            this.cancel.Text = "Cancel";
            this.cancel.UseVisualStyleBackColor = true;
            this.cancel.Click += new System.EventHandler(this.cancel_Click);
            // 
            // IssueForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(638, 435);
            this.Controls.Add(this.cancel);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.create);
            this.Controls.Add(this.issueFileBrowse);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.issueFilePath);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "IssueForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Issue identification";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.CheckBox pbVerbsInVnNotSl;
        private System.Windows.Forms.Button issueFileBrowse;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox issueFilePath;
        private System.Windows.Forms.Button create;
        private System.Windows.Forms.CheckBox pbVerbsInvalidVnClass;
        private System.Windows.Forms.CheckBox invalidVnRole;
        private System.Windows.Forms.CheckBox pbVerbsNotInVN;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button cancel;
        private System.Windows.Forms.CheckBox includeFN;
        private System.Windows.Forms.CheckBox includePB;
        private System.Windows.Forms.CheckBox propBankReVerbs;
        private System.Windows.Forms.CheckBox fnFramesInvalidVnClass;
        private System.Windows.Forms.CheckBox invalidFrameElements;
        private System.Windows.Forms.CheckBox invalidPropBankRoles;
    }
}