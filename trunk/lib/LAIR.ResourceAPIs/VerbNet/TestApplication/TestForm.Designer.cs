namespace TestApplication
{
    partial class TestForm
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
            this.classCombo = new System.Windows.Forms.ComboBox();
            this.getVerbsInClassBtn = new System.Windows.Forms.Button();
            this.items = new System.Windows.Forms.ListBox();
            this.recursiveChk = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.getClassesBtn = new System.Windows.Forms.Button();
            this.verbBox = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 19);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(76, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "VerbNet class:";
            // 
            // classCombo
            // 
            this.classCombo.DropDownHeight = 250;
            this.classCombo.FormattingEnabled = true;
            this.classCombo.IntegralHeight = false;
            this.classCombo.Location = new System.Drawing.Point(94, 16);
            this.classCombo.Name = "classCombo";
            this.classCombo.Size = new System.Drawing.Size(172, 21);
            this.classCombo.Sorted = true;
            this.classCombo.TabIndex = 0;
            // 
            // getVerbsInClassBtn
            // 
            this.getVerbsInClassBtn.Location = new System.Drawing.Point(272, 14);
            this.getVerbsInClassBtn.Name = "getVerbsInClassBtn";
            this.getVerbsInClassBtn.Size = new System.Drawing.Size(115, 23);
            this.getVerbsInClassBtn.TabIndex = 1;
            this.getVerbsInClassBtn.Text = "Get verbs in class";
            this.getVerbsInClassBtn.UseVisualStyleBackColor = true;
            this.getVerbsInClassBtn.Click += new System.EventHandler(this.getVerbsInClassBtn_Click);
            // 
            // items
            // 
            this.items.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.items.FormattingEnabled = true;
            this.items.Location = new System.Drawing.Point(12, 94);
            this.items.Name = "items";
            this.items.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.items.Size = new System.Drawing.Size(375, 381);
            this.items.TabIndex = 5;
            this.items.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.itemList_MouseDoubleClick);
            // 
            // recursiveChk
            // 
            this.recursiveChk.AutoSize = true;
            this.recursiveChk.Location = new System.Drawing.Point(272, 43);
            this.recursiveChk.Name = "recursiveChk";
            this.recursiveChk.Size = new System.Drawing.Size(74, 17);
            this.recursiveChk.TabIndex = 2;
            this.recursiveChk.Text = "Recursive";
            this.recursiveChk.UseVisualStyleBackColor = true;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(56, 71);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(32, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "Verb:";
            // 
            // getClassesBtn
            // 
            this.getClassesBtn.Location = new System.Drawing.Point(272, 66);
            this.getClassesBtn.Name = "getClassesBtn";
            this.getClassesBtn.Size = new System.Drawing.Size(115, 23);
            this.getClassesBtn.TabIndex = 4;
            this.getClassesBtn.Text = "Get classes for verb";
            this.getClassesBtn.UseVisualStyleBackColor = true;
            this.getClassesBtn.Click += new System.EventHandler(this.getClassesBtn_Click);
            // 
            // verbBox
            // 
            this.verbBox.Location = new System.Drawing.Point(94, 68);
            this.verbBox.Name = "verbBox";
            this.verbBox.Size = new System.Drawing.Size(172, 20);
            this.verbBox.TabIndex = 3;
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(402, 485);
            this.Controls.Add(this.verbBox);
            this.Controls.Add(this.getClassesBtn);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.recursiveChk);
            this.Controls.Add(this.items);
            this.Controls.Add(this.getVerbsInClassBtn);
            this.Controls.Add(this.classCombo);
            this.Controls.Add(this.label1);
            this.Name = "TestForm";
            this.Text = "VerbNet test application";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox classCombo;
        private System.Windows.Forms.Button getVerbsInClassBtn;
        private System.Windows.Forms.ListBox items;
        private System.Windows.Forms.CheckBox recursiveChk;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button getClassesBtn;
        private System.Windows.Forms.TextBox verbBox;
    }
}

