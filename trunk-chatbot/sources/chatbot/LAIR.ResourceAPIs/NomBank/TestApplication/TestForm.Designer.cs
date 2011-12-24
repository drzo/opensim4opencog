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
            this.nomBankGroup = new System.Windows.Forms.GroupBox();
            this.includeNounSense = new System.Windows.Forms.CheckBox();
            this.nounCombo = new System.Windows.Forms.ComboBox();
            this.getFrameBtn = new System.Windows.Forms.Button();
            this.nodeTypeCombo = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.getNodesByTypeBtn = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.getNounAttBtn = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.nodeFeatureCombo = new System.Windows.Forms.ComboBox();
            this.testAllNounsBtn = new System.Windows.Forms.Button();
            this.getNodesByFeatureBtn = new System.Windows.Forms.Button();
            this.label6 = new System.Windows.Forms.Label();
            this.numItemsLbl = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.getClassesForNounBtn = new System.Windows.Forms.Button();
            this.nomLexNounCombo = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.allowAmbiguousNomsChk = new System.Windows.Forms.CheckBox();
            this.getNounsInClassBtn = new System.Windows.Forms.Button();
            this.classCombo = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.output = new System.Windows.Forms.TextBox();
            this.filter = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.nomBankGroup.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // nomBankGroup
            // 
            this.nomBankGroup.Controls.Add(this.includeNounSense);
            this.nomBankGroup.Controls.Add(this.nounCombo);
            this.nomBankGroup.Controls.Add(this.getFrameBtn);
            this.nomBankGroup.Controls.Add(this.nodeTypeCombo);
            this.nomBankGroup.Controls.Add(this.label5);
            this.nomBankGroup.Controls.Add(this.getNodesByTypeBtn);
            this.nomBankGroup.Controls.Add(this.label1);
            this.nomBankGroup.Controls.Add(this.getNounAttBtn);
            this.nomBankGroup.Controls.Add(this.label3);
            this.nomBankGroup.Controls.Add(this.nodeFeatureCombo);
            this.nomBankGroup.Controls.Add(this.testAllNounsBtn);
            this.nomBankGroup.Controls.Add(this.getNodesByFeatureBtn);
            this.nomBankGroup.Location = new System.Drawing.Point(12, 12);
            this.nomBankGroup.Name = "nomBankGroup";
            this.nomBankGroup.Size = new System.Drawing.Size(473, 182);
            this.nomBankGroup.TabIndex = 0;
            this.nomBankGroup.TabStop = false;
            this.nomBankGroup.Text = "NomBank";
            // 
            // includeNounSense
            // 
            this.includeNounSense.AutoSize = true;
            this.includeNounSense.Location = new System.Drawing.Point(232, 51);
            this.includeNounSense.Name = "includeNounSense";
            this.includeNounSense.Size = new System.Drawing.Size(97, 17);
            this.includeNounSense.TabIndex = 15;
            this.includeNounSense.Text = "Include senses";
            this.includeNounSense.UseVisualStyleBackColor = true;
            // 
            // nounCombo
            // 
            this.nounCombo.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.nounCombo.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.nounCombo.DropDownHeight = 300;
            this.nounCombo.FormattingEnabled = true;
            this.nounCombo.IntegralHeight = false;
            this.nounCombo.Location = new System.Drawing.Point(76, 24);
            this.nounCombo.Name = "nounCombo";
            this.nounCombo.Size = new System.Drawing.Size(150, 21);
            this.nounCombo.TabIndex = 14;
            // 
            // getFrameBtn
            // 
            this.getFrameBtn.Location = new System.Drawing.Point(363, 22);
            this.getFrameBtn.Name = "getFrameBtn";
            this.getFrameBtn.Size = new System.Drawing.Size(94, 23);
            this.getFrameBtn.TabIndex = 2;
            this.getFrameBtn.Text = "Get frame";
            this.getFrameBtn.UseVisualStyleBackColor = true;
            this.getFrameBtn.Click += new System.EventHandler(this.getFrameBtn_Click);
            // 
            // nodeTypeCombo
            // 
            this.nodeTypeCombo.FormattingEnabled = true;
            this.nodeTypeCombo.Location = new System.Drawing.Point(76, 80);
            this.nodeTypeCombo.Name = "nodeTypeCombo";
            this.nodeTypeCombo.Size = new System.Drawing.Size(150, 21);
            this.nodeTypeCombo.TabIndex = 3;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(11, 83);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(59, 13);
            this.label5.TabIndex = 13;
            this.label5.Text = "Node type:";
            this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // getNodesByTypeBtn
            // 
            this.getNodesByTypeBtn.Location = new System.Drawing.Point(232, 78);
            this.getNodesByTypeBtn.Name = "getNodesByTypeBtn";
            this.getNodesByTypeBtn.Size = new System.Drawing.Size(125, 23);
            this.getNodesByTypeBtn.TabIndex = 4;
            this.getNodesByTypeBtn.Text = "Get nodes by type";
            this.getNodesByTypeBtn.UseVisualStyleBackColor = true;
            this.getNodesByTypeBtn.Click += new System.EventHandler(this.getNodesByTypeBtn_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(34, 27);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(36, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Noun:";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // getNounAttBtn
            // 
            this.getNounAttBtn.Location = new System.Drawing.Point(232, 22);
            this.getNounAttBtn.Name = "getNounAttBtn";
            this.getNounAttBtn.Size = new System.Drawing.Size(125, 23);
            this.getNounAttBtn.TabIndex = 1;
            this.getNounAttBtn.Text = "Get attestations";
            this.getNounAttBtn.UseVisualStyleBackColor = true;
            this.getNounAttBtn.Click += new System.EventHandler(this.getNounAttBtn_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(24, 110);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(46, 13);
            this.label3.TabIndex = 11;
            this.label3.Text = "Feature:";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // nodeFeatureCombo
            // 
            this.nodeFeatureCombo.FormattingEnabled = true;
            this.nodeFeatureCombo.Location = new System.Drawing.Point(76, 107);
            this.nodeFeatureCombo.Name = "nodeFeatureCombo";
            this.nodeFeatureCombo.Size = new System.Drawing.Size(150, 21);
            this.nodeFeatureCombo.TabIndex = 5;
            // 
            // testAllNounsBtn
            // 
            this.testAllNounsBtn.Location = new System.Drawing.Point(42, 153);
            this.testAllNounsBtn.Name = "testAllNounsBtn";
            this.testAllNounsBtn.Size = new System.Drawing.Size(392, 23);
            this.testAllNounsBtn.TabIndex = 7;
            this.testAllNounsBtn.Text = "Test retrieval of all indexed attestations for all nouns (will take a while to fi" +
                "nish)";
            this.testAllNounsBtn.UseVisualStyleBackColor = true;
            this.testAllNounsBtn.Click += new System.EventHandler(this.testAllNounsBtn_Click);
            // 
            // getNodesByFeatureBtn
            // 
            this.getNodesByFeatureBtn.Location = new System.Drawing.Point(232, 105);
            this.getNodesByFeatureBtn.Name = "getNodesByFeatureBtn";
            this.getNodesByFeatureBtn.Size = new System.Drawing.Size(125, 23);
            this.getNodesByFeatureBtn.TabIndex = 6;
            this.getNodesByFeatureBtn.Text = "Get nodes by feature";
            this.getNodesByFeatureBtn.UseVisualStyleBackColor = true;
            this.getNodesByFeatureBtn.Click += new System.EventHandler(this.getNodesByFeatureBtn_Click);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(14, 209);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(62, 20);
            this.label6.TabIndex = 17;
            this.label6.Text = "Output:";
            // 
            // numItemsLbl
            // 
            this.numItemsLbl.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.numItemsLbl.AutoSize = true;
            this.numItemsLbl.Location = new System.Drawing.Point(805, 214);
            this.numItemsLbl.Name = "numItemsLbl";
            this.numItemsLbl.Size = new System.Drawing.Size(40, 13);
            this.numItemsLbl.TabIndex = 16;
            this.numItemsLbl.Text = "0 items";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.getClassesForNounBtn);
            this.groupBox1.Controls.Add(this.nomLexNounCombo);
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.allowAmbiguousNomsChk);
            this.groupBox1.Controls.Add(this.getNounsInClassBtn);
            this.groupBox1.Controls.Add(this.classCombo);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Location = new System.Drawing.Point(491, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(321, 152);
            this.groupBox1.TabIndex = 18;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "NomLex";
            // 
            // getClassesForNounBtn
            // 
            this.getClassesForNounBtn.Location = new System.Drawing.Point(181, 75);
            this.getClassesForNounBtn.Name = "getClassesForNounBtn";
            this.getClassesForNounBtn.Size = new System.Drawing.Size(125, 23);
            this.getClassesForNounBtn.TabIndex = 19;
            this.getClassesForNounBtn.Text = "Get classes for noun";
            this.getClassesForNounBtn.UseVisualStyleBackColor = true;
            this.getClassesForNounBtn.Click += new System.EventHandler(this.getClassesForNounBtn_Click);
            // 
            // nomLexNounCombo
            // 
            this.nomLexNounCombo.FormattingEnabled = true;
            this.nomLexNounCombo.Location = new System.Drawing.Point(56, 77);
            this.nomLexNounCombo.Name = "nomLexNounCombo";
            this.nomLexNounCombo.Size = new System.Drawing.Size(119, 21);
            this.nomLexNounCombo.TabIndex = 20;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(14, 80);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(36, 13);
            this.label4.TabIndex = 18;
            this.label4.Text = "Noun:";
            this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // allowAmbiguousNomsChk
            // 
            this.allowAmbiguousNomsChk.AutoSize = true;
            this.allowAmbiguousNomsChk.Location = new System.Drawing.Point(181, 51);
            this.allowAmbiguousNomsChk.Name = "allowAmbiguousNomsChk";
            this.allowAmbiguousNomsChk.Size = new System.Drawing.Size(115, 17);
            this.allowAmbiguousNomsChk.TabIndex = 17;
            this.allowAmbiguousNomsChk.Text = "Include ambiguous";
            this.allowAmbiguousNomsChk.UseVisualStyleBackColor = true;
            // 
            // getNounsInClassBtn
            // 
            this.getNounsInClassBtn.Location = new System.Drawing.Point(181, 22);
            this.getNounsInClassBtn.Name = "getNounsInClassBtn";
            this.getNounsInClassBtn.Size = new System.Drawing.Size(125, 23);
            this.getNounsInClassBtn.TabIndex = 15;
            this.getNounsInClassBtn.Text = "Get nouns in class";
            this.getNounsInClassBtn.UseVisualStyleBackColor = true;
            this.getNounsInClassBtn.Click += new System.EventHandler(this.getNounsInClassBtn_Click);
            // 
            // classCombo
            // 
            this.classCombo.FormattingEnabled = true;
            this.classCombo.Location = new System.Drawing.Point(56, 24);
            this.classCombo.Name = "classCombo";
            this.classCombo.Size = new System.Drawing.Size(119, 21);
            this.classCombo.TabIndex = 16;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(15, 27);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(35, 13);
            this.label2.TabIndex = 15;
            this.label2.Text = "Class:";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // output
            // 
            this.output.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.output.Location = new System.Drawing.Point(18, 232);
            this.output.Multiline = true;
            this.output.Name = "output";
            this.output.ReadOnly = true;
            this.output.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output.Size = new System.Drawing.Size(844, 344);
            this.output.TabIndex = 19;
            this.output.WordWrap = false;
            // 
            // filter
            // 
            this.filter.Location = new System.Drawing.Point(314, 211);
            this.filter.Name = "filter";
            this.filter.Size = new System.Drawing.Size(352, 20);
            this.filter.TabIndex = 24;
            this.filter.TextChanged += new System.EventHandler(this.filter_TextChanged);
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(276, 214);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(32, 13);
            this.label7.TabIndex = 23;
            this.label7.Text = "Filter:";
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(874, 588);
            this.Controls.Add(this.filter);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.output);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.numItemsLbl);
            this.Controls.Add(this.nomBankGroup);
            this.Name = "TestForm";
            this.Text = "NomBank test application";
            this.nomBankGroup.ResumeLayout(false);
            this.nomBankGroup.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox nomBankGroup;
        private System.Windows.Forms.Button getFrameBtn;
        private System.Windows.Forms.ComboBox nodeTypeCombo;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button getNodesByTypeBtn;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button getNounAttBtn;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox nodeFeatureCombo;
        private System.Windows.Forms.Button testAllNounsBtn;
        private System.Windows.Forms.Button getNodesByFeatureBtn;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label numItemsLbl;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.ComboBox nounCombo;
        private System.Windows.Forms.Button getNounsInClassBtn;
        private System.Windows.Forms.ComboBox classCombo;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.CheckBox allowAmbiguousNomsChk;
        private System.Windows.Forms.Button getClassesForNounBtn;
        private System.Windows.Forms.ComboBox nomLexNounCombo;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.CheckBox includeNounSense;
        private System.Windows.Forms.TextBox output;
        private System.Windows.Forms.TextBox filter;
        private System.Windows.Forms.Label label7;
    }
}

