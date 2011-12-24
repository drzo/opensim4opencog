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
            this.getVerbAttBtn = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.testAllVerbsBtn = new System.Windows.Forms.Button();
            this.numItemsLbl = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.nodeFeatureCombo = new System.Windows.Forms.ComboBox();
            this.getNodesByFeatureBtn = new System.Windows.Forms.Button();
            this.treeBankGroupBox = new System.Windows.Forms.GroupBox();
            this.search = new System.Windows.Forms.CheckBox();
            this.categoryTags = new System.Windows.Forms.CheckBox();
            this.leavesOnly = new System.Windows.Forms.RadioButton();
            this.displayParseTrees = new System.Windows.Forms.RadioButton();
            this.mrgFileCombo = new System.Windows.Forms.ComboBox();
            this.useCategoryMnemonic = new System.Windows.Forms.CheckBox();
            this.displayPhraseHeads = new System.Windows.Forms.CheckBox();
            this.testTBIndexBtn = new System.Windows.Forms.Button();
            this.getTbSentences = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.propBankGroupBox = new System.Windows.Forms.GroupBox();
            this.verbCombo = new System.Windows.Forms.ComboBox();
            this.getFrameBtn = new System.Windows.Forms.Button();
            this.nodeTypeCombo = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.getNodesByTypeBtn = new System.Windows.Forms.Button();
            this.label6 = new System.Windows.Forms.Label();
            this.fileBrowser = new System.Windows.Forms.OpenFileDialog();
            this.loadTbBtn = new System.Windows.Forms.Button();
            this.loadPbBtn = new System.Windows.Forms.Button();
            this.output = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.filter = new System.Windows.Forms.TextBox();
            this.validFilter = new System.Windows.Forms.Label();
            this.treeBankGroupBox.SuspendLayout();
            this.propBankGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // getVerbAttBtn
            // 
            this.getVerbAttBtn.Location = new System.Drawing.Point(255, 23);
            this.getVerbAttBtn.Name = "getVerbAttBtn";
            this.getVerbAttBtn.Size = new System.Drawing.Size(143, 23);
            this.getVerbAttBtn.TabIndex = 0;
            this.getVerbAttBtn.Text = "Get attestations";
            this.getVerbAttBtn.UseVisualStyleBackColor = true;
            this.getVerbAttBtn.Click += new System.EventHandler(this.getVerbAttBtn_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(39, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(32, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Verb:";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // testAllVerbsBtn
            // 
            this.testAllVerbsBtn.Location = new System.Drawing.Point(90, 123);
            this.testAllVerbsBtn.Name = "testAllVerbsBtn";
            this.testAllVerbsBtn.Size = new System.Drawing.Size(392, 23);
            this.testAllVerbsBtn.TabIndex = 4;
            this.testAllVerbsBtn.Text = "Test retrieval of all indexed attestations for all verbs (will take a while to fi" +
                "nish)";
            this.testAllVerbsBtn.UseVisualStyleBackColor = true;
            this.testAllVerbsBtn.Click += new System.EventHandler(this.testAllVerbsBtn_Click);
            // 
            // numItemsLbl
            // 
            this.numItemsLbl.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.numItemsLbl.AutoSize = true;
            this.numItemsLbl.Location = new System.Drawing.Point(934, 215);
            this.numItemsLbl.Name = "numItemsLbl";
            this.numItemsLbl.Size = new System.Drawing.Size(40, 13);
            this.numItemsLbl.TabIndex = 7;
            this.numItemsLbl.Text = "0 items";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(25, 80);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(46, 13);
            this.label3.TabIndex = 11;
            this.label3.Text = "Feature:";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // nodeFeatureCombo
            // 
            this.nodeFeatureCombo.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.nodeFeatureCombo.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.nodeFeatureCombo.FormattingEnabled = true;
            this.nodeFeatureCombo.Location = new System.Drawing.Point(77, 78);
            this.nodeFeatureCombo.Name = "nodeFeatureCombo";
            this.nodeFeatureCombo.Size = new System.Drawing.Size(172, 21);
            this.nodeFeatureCombo.TabIndex = 10;
            // 
            // getNodesByFeatureBtn
            // 
            this.getNodesByFeatureBtn.Location = new System.Drawing.Point(255, 76);
            this.getNodesByFeatureBtn.Name = "getNodesByFeatureBtn";
            this.getNodesByFeatureBtn.Size = new System.Drawing.Size(143, 23);
            this.getNodesByFeatureBtn.TabIndex = 9;
            this.getNodesByFeatureBtn.Text = "Get nodes by feature";
            this.getNodesByFeatureBtn.UseVisualStyleBackColor = true;
            this.getNodesByFeatureBtn.Click += new System.EventHandler(this.getNodesByFeatureBtn_Click);
            // 
            // treeBankGroupBox
            // 
            this.treeBankGroupBox.Controls.Add(this.search);
            this.treeBankGroupBox.Controls.Add(this.categoryTags);
            this.treeBankGroupBox.Controls.Add(this.leavesOnly);
            this.treeBankGroupBox.Controls.Add(this.displayParseTrees);
            this.treeBankGroupBox.Controls.Add(this.mrgFileCombo);
            this.treeBankGroupBox.Controls.Add(this.useCategoryMnemonic);
            this.treeBankGroupBox.Controls.Add(this.displayPhraseHeads);
            this.treeBankGroupBox.Controls.Add(this.testTBIndexBtn);
            this.treeBankGroupBox.Controls.Add(this.getTbSentences);
            this.treeBankGroupBox.Controls.Add(this.label4);
            this.treeBankGroupBox.Enabled = false;
            this.treeBankGroupBox.Location = new System.Drawing.Point(12, 36);
            this.treeBankGroupBox.Name = "treeBankGroupBox";
            this.treeBankGroupBox.Size = new System.Drawing.Size(490, 160);
            this.treeBankGroupBox.TabIndex = 12;
            this.treeBankGroupBox.TabStop = false;
            this.treeBankGroupBox.Text = "TreeBank";
            // 
            // search
            // 
            this.search.AutoSize = true;
            this.search.Location = new System.Drawing.Point(53, 137);
            this.search.Name = "search";
            this.search.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.search.Size = new System.Drawing.Size(101, 17);
            this.search.TabIndex = 26;
            this.search.Text = "Conduct search";
            this.search.UseVisualStyleBackColor = true;
            // 
            // categoryTags
            // 
            this.categoryTags.AutoSize = true;
            this.categoryTags.Checked = true;
            this.categoryTags.CheckState = System.Windows.Forms.CheckState.Checked;
            this.categoryTags.Enabled = false;
            this.categoryTags.Location = new System.Drawing.Point(212, 78);
            this.categoryTags.Name = "categoryTags";
            this.categoryTags.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.categoryTags.Size = new System.Drawing.Size(91, 17);
            this.categoryTags.TabIndex = 25;
            this.categoryTags.Text = "Category tags";
            this.categoryTags.UseVisualStyleBackColor = true;
            // 
            // leavesOnly
            // 
            this.leavesOnly.AutoSize = true;
            this.leavesOnly.Location = new System.Drawing.Point(221, 55);
            this.leavesOnly.Name = "leavesOnly";
            this.leavesOnly.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.leavesOnly.Size = new System.Drawing.Size(82, 17);
            this.leavesOnly.TabIndex = 24;
            this.leavesOnly.Text = "Leaves only";
            this.leavesOnly.UseVisualStyleBackColor = true;
            this.leavesOnly.CheckedChanged += new System.EventHandler(this.leavesOnly_CheckedChanged);
            // 
            // displayParseTrees
            // 
            this.displayParseTrees.AutoSize = true;
            this.displayParseTrees.Checked = true;
            this.displayParseTrees.Location = new System.Drawing.Point(69, 55);
            this.displayParseTrees.Name = "displayParseTrees";
            this.displayParseTrees.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.displayParseTrees.Size = new System.Drawing.Size(114, 17);
            this.displayParseTrees.TabIndex = 23;
            this.displayParseTrees.TabStop = true;
            this.displayParseTrees.Text = "Display parse trees";
            this.displayParseTrees.UseVisualStyleBackColor = true;
            this.displayParseTrees.CheckedChanged += new System.EventHandler(this.displayParseTrees_CheckedChanged);
            // 
            // mrgFileCombo
            // 
            this.mrgFileCombo.DropDownHeight = 300;
            this.mrgFileCombo.FormattingEnabled = true;
            this.mrgFileCombo.IntegralHeight = false;
            this.mrgFileCombo.Location = new System.Drawing.Point(123, 20);
            this.mrgFileCombo.Name = "mrgFileCombo";
            this.mrgFileCombo.Size = new System.Drawing.Size(201, 21);
            this.mrgFileCombo.TabIndex = 21;
            // 
            // useCategoryMnemonic
            // 
            this.useCategoryMnemonic.AutoSize = true;
            this.useCategoryMnemonic.Checked = true;
            this.useCategoryMnemonic.CheckState = System.Windows.Forms.CheckState.Checked;
            this.useCategoryMnemonic.Location = new System.Drawing.Point(330, 56);
            this.useCategoryMnemonic.Name = "useCategoryMnemonic";
            this.useCategoryMnemonic.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.useCategoryMnemonic.Size = new System.Drawing.Size(145, 17);
            this.useCategoryMnemonic.TabIndex = 20;
            this.useCategoryMnemonic.Text = "Use category mnemonics";
            this.useCategoryMnemonic.UseVisualStyleBackColor = true;
            // 
            // displayPhraseHeads
            // 
            this.displayPhraseHeads.AutoSize = true;
            this.displayPhraseHeads.Checked = true;
            this.displayPhraseHeads.CheckState = System.Windows.Forms.CheckState.Checked;
            this.displayPhraseHeads.Location = new System.Drawing.Point(56, 78);
            this.displayPhraseHeads.Name = "displayPhraseHeads";
            this.displayPhraseHeads.RightToLeft = System.Windows.Forms.RightToLeft.Yes;
            this.displayPhraseHeads.Size = new System.Drawing.Size(127, 17);
            this.displayPhraseHeads.TabIndex = 19;
            this.displayPhraseHeads.Text = "Display phrase heads";
            this.displayPhraseHeads.UseVisualStyleBackColor = true;
            // 
            // testTBIndexBtn
            // 
            this.testTBIndexBtn.Location = new System.Drawing.Point(53, 108);
            this.testTBIndexBtn.Name = "testTBIndexBtn";
            this.testTBIndexBtn.Size = new System.Drawing.Size(392, 23);
            this.testTBIndexBtn.TabIndex = 12;
            this.testTBIndexBtn.Text = "Test retrieval of all indexed sentences (will take a while to finish)";
            this.testTBIndexBtn.UseVisualStyleBackColor = true;
            this.testTBIndexBtn.Click += new System.EventHandler(this.testTBIndexBtn_Click);
            // 
            // getTbSentences
            // 
            this.getTbSentences.Location = new System.Drawing.Point(330, 18);
            this.getTbSentences.Name = "getTbSentences";
            this.getTbSentences.Size = new System.Drawing.Size(89, 23);
            this.getTbSentences.TabIndex = 12;
            this.getTbSentences.Text = "Get sentences";
            this.getTbSentences.UseVisualStyleBackColor = true;
            this.getTbSentences.Click += new System.EventHandler(this.getTbSentences_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(66, 23);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(51, 13);
            this.label4.TabIndex = 13;
            this.label4.Text = "MRG file:";
            this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // propBankGroupBox
            // 
            this.propBankGroupBox.Controls.Add(this.verbCombo);
            this.propBankGroupBox.Controls.Add(this.getFrameBtn);
            this.propBankGroupBox.Controls.Add(this.nodeTypeCombo);
            this.propBankGroupBox.Controls.Add(this.label5);
            this.propBankGroupBox.Controls.Add(this.getNodesByTypeBtn);
            this.propBankGroupBox.Controls.Add(this.label1);
            this.propBankGroupBox.Controls.Add(this.getVerbAttBtn);
            this.propBankGroupBox.Controls.Add(this.label3);
            this.propBankGroupBox.Controls.Add(this.nodeFeatureCombo);
            this.propBankGroupBox.Controls.Add(this.testAllVerbsBtn);
            this.propBankGroupBox.Controls.Add(this.getNodesByFeatureBtn);
            this.propBankGroupBox.Enabled = false;
            this.propBankGroupBox.Location = new System.Drawing.Point(508, 36);
            this.propBankGroupBox.Name = "propBankGroupBox";
            this.propBankGroupBox.Size = new System.Drawing.Size(511, 160);
            this.propBankGroupBox.TabIndex = 13;
            this.propBankGroupBox.TabStop = false;
            this.propBankGroupBox.Text = "PropBank";
            // 
            // verbCombo
            // 
            this.verbCombo.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.verbCombo.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.verbCombo.FormattingEnabled = true;
            this.verbCombo.Location = new System.Drawing.Point(77, 25);
            this.verbCombo.Name = "verbCombo";
            this.verbCombo.Size = new System.Drawing.Size(172, 21);
            this.verbCombo.TabIndex = 16;
            // 
            // getFrameBtn
            // 
            this.getFrameBtn.Location = new System.Drawing.Point(404, 23);
            this.getFrameBtn.Name = "getFrameBtn";
            this.getFrameBtn.Size = new System.Drawing.Size(94, 23);
            this.getFrameBtn.TabIndex = 15;
            this.getFrameBtn.Text = "Get frame";
            this.getFrameBtn.UseVisualStyleBackColor = true;
            this.getFrameBtn.Click += new System.EventHandler(this.getFrameBtn_Click);
            // 
            // nodeTypeCombo
            // 
            this.nodeTypeCombo.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.nodeTypeCombo.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.nodeTypeCombo.FormattingEnabled = true;
            this.nodeTypeCombo.Location = new System.Drawing.Point(77, 51);
            this.nodeTypeCombo.Name = "nodeTypeCombo";
            this.nodeTypeCombo.Size = new System.Drawing.Size(172, 21);
            this.nodeTypeCombo.TabIndex = 14;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(12, 54);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(59, 13);
            this.label5.TabIndex = 13;
            this.label5.Text = "Node type:";
            this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // getNodesByTypeBtn
            // 
            this.getNodesByTypeBtn.Location = new System.Drawing.Point(255, 49);
            this.getNodesByTypeBtn.Name = "getNodesByTypeBtn";
            this.getNodesByTypeBtn.Size = new System.Drawing.Size(143, 23);
            this.getNodesByTypeBtn.TabIndex = 12;
            this.getNodesByTypeBtn.Text = "Get nodes by type";
            this.getNodesByTypeBtn.UseVisualStyleBackColor = true;
            this.getNodesByTypeBtn.Click += new System.EventHandler(this.getNodesByTypeBtn_Click);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(12, 210);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(62, 20);
            this.label6.TabIndex = 14;
            this.label6.Text = "Output:";
            // 
            // fileBrowser
            // 
            this.fileBrowser.Title = "Browse for file...";
            // 
            // loadTbBtn
            // 
            this.loadTbBtn.Location = new System.Drawing.Point(12, 7);
            this.loadTbBtn.Name = "loadTbBtn";
            this.loadTbBtn.Size = new System.Drawing.Size(132, 23);
            this.loadTbBtn.TabIndex = 18;
            this.loadTbBtn.Text = "Load TreeBank";
            this.loadTbBtn.UseVisualStyleBackColor = true;
            this.loadTbBtn.Click += new System.EventHandler(this.loadTbBtn_Click);
            // 
            // loadPbBtn
            // 
            this.loadPbBtn.Location = new System.Drawing.Point(508, 7);
            this.loadPbBtn.Name = "loadPbBtn";
            this.loadPbBtn.Size = new System.Drawing.Size(132, 23);
            this.loadPbBtn.TabIndex = 19;
            this.loadPbBtn.Text = "Load PropBank";
            this.loadPbBtn.UseVisualStyleBackColor = true;
            this.loadPbBtn.Click += new System.EventHandler(this.loadPbBtn_Click);
            // 
            // output
            // 
            this.output.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.output.Location = new System.Drawing.Point(12, 233);
            this.output.Multiline = true;
            this.output.Name = "output";
            this.output.ReadOnly = true;
            this.output.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output.Size = new System.Drawing.Size(1008, 344);
            this.output.TabIndex = 20;
            this.output.WordWrap = false;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(295, 215);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(32, 13);
            this.label2.TabIndex = 21;
            this.label2.Text = "Filter:";
            // 
            // filter
            // 
            this.filter.Location = new System.Drawing.Point(333, 212);
            this.filter.Name = "filter";
            this.filter.Size = new System.Drawing.Size(352, 20);
            this.filter.TabIndex = 22;
            this.filter.TextChanged += new System.EventHandler(this.filter_TextChanged);
            // 
            // validFilter
            // 
            this.validFilter.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.validFilter.AutoSize = true;
            this.validFilter.Location = new System.Drawing.Point(691, 215);
            this.validFilter.Name = "validFilter";
            this.validFilter.Size = new System.Drawing.Size(30, 13);
            this.validFilter.TabIndex = 23;
            this.validFilter.Text = "Valid";
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1032, 589);
            this.Controls.Add(this.validFilter);
            this.Controls.Add(this.filter);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.output);
            this.Controls.Add(this.loadPbBtn);
            this.Controls.Add(this.loadTbBtn);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.propBankGroupBox);
            this.Controls.Add(this.treeBankGroupBox);
            this.Controls.Add(this.numItemsLbl);
            this.Name = "TestForm";
            this.Text = "PennBank test application";
            this.treeBankGroupBox.ResumeLayout(false);
            this.treeBankGroupBox.PerformLayout();
            this.propBankGroupBox.ResumeLayout(false);
            this.propBankGroupBox.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button getVerbAttBtn;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button testAllVerbsBtn;
        private System.Windows.Forms.Label numItemsLbl;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox nodeFeatureCombo;
        private System.Windows.Forms.Button getNodesByFeatureBtn;
        private System.Windows.Forms.GroupBox treeBankGroupBox;
        private System.Windows.Forms.GroupBox propBankGroupBox;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.OpenFileDialog fileBrowser;
        private System.Windows.Forms.Button getTbSentences;
        private System.Windows.Forms.Button testTBIndexBtn;
        private System.Windows.Forms.ComboBox nodeTypeCombo;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button getNodesByTypeBtn;
        private System.Windows.Forms.Button getFrameBtn;
        private System.Windows.Forms.Button loadTbBtn;
        private System.Windows.Forms.Button loadPbBtn;
        private System.Windows.Forms.TextBox output;
        private System.Windows.Forms.CheckBox useCategoryMnemonic;
        private System.Windows.Forms.CheckBox displayPhraseHeads;
        private System.Windows.Forms.ComboBox mrgFileCombo;
        private System.Windows.Forms.ComboBox verbCombo;
        private System.Windows.Forms.RadioButton displayParseTrees;
        private System.Windows.Forms.RadioButton leavesOnly;
        private System.Windows.Forms.CheckBox categoryTags;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox filter;
        private System.Windows.Forms.CheckBox search;
        private System.Windows.Forms.Label validFilter;
    }
}

