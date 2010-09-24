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
            this.getAttestationsForFrame1 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.getLexicalUnitsForFrame1 = new System.Windows.Forms.Button();
            this.getFramesRelatedToFrame1 = new System.Windows.Forms.Button();
            this.getFrame1Frame2FeMapping = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.recursive = new System.Windows.Forms.CheckBox();
            this.numItemsLbl = new System.Windows.Forms.Label();
            this.checkFrames = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.lexicalUnit = new System.Windows.Forms.TextBox();
            this.getAttestationsForLexicalUnit = new System.Windows.Forms.Button();
            this.both = new System.Windows.Forms.RadioButton();
            this.super = new System.Windows.Forms.RadioButton();
            this.sub = new System.Windows.Forms.RadioButton();
            this.lexicalUnitPOS = new System.Windows.Forms.TextBox();
            this.getFramesForLexicalUnit = new System.Windows.Forms.Button();
            this.label6 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.relations = new System.Windows.Forms.ListBox();
            this.frame1 = new System.Windows.Forms.ComboBox();
            this.fe1 = new System.Windows.Forms.ComboBox();
            this.label8 = new System.Windows.Forms.Label();
            this.fe2 = new System.Windows.Forms.ComboBox();
            this.label9 = new System.Windows.Forms.Label();
            this.frame2 = new System.Windows.Forms.ComboBox();
            this.getFe1Fe2Path = new System.Windows.Forms.Button();
            this.output = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // getAttestationsForFrame1
            // 
            this.getAttestationsForFrame1.Location = new System.Drawing.Point(350, 141);
            this.getAttestationsForFrame1.Name = "getAttestationsForFrame1";
            this.getAttestationsForFrame1.Size = new System.Drawing.Size(103, 23);
            this.getAttestationsForFrame1.TabIndex = 6;
            this.getAttestationsForFrame1.Text = "Get attestations";
            this.getAttestationsForFrame1.UseVisualStyleBackColor = true;
            this.getAttestationsForFrame1.Click += new System.EventHandler(this.getAttestationsForFrame1_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(27, 146);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(48, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "Frame 1:";
            // 
            // getLexicalUnitsForFrame1
            // 
            this.getLexicalUnitsForFrame1.Location = new System.Drawing.Point(241, 141);
            this.getLexicalUnitsForFrame1.Name = "getLexicalUnitsForFrame1";
            this.getLexicalUnitsForFrame1.Size = new System.Drawing.Size(103, 23);
            this.getLexicalUnitsForFrame1.TabIndex = 5;
            this.getLexicalUnitsForFrame1.Text = "Get lexical units";
            this.getLexicalUnitsForFrame1.UseVisualStyleBackColor = true;
            this.getLexicalUnitsForFrame1.Click += new System.EventHandler(this.getLexicalUnitsForFrame1_Click);
            // 
            // getFramesRelatedToFrame1
            // 
            this.getFramesRelatedToFrame1.Location = new System.Drawing.Point(81, 380);
            this.getFramesRelatedToFrame1.Name = "getFramesRelatedToFrame1";
            this.getFramesRelatedToFrame1.Size = new System.Drawing.Size(154, 23);
            this.getFramesRelatedToFrame1.TabIndex = 13;
            this.getFramesRelatedToFrame1.Text = "Get frames related to frame 1";
            this.getFramesRelatedToFrame1.UseVisualStyleBackColor = true;
            this.getFramesRelatedToFrame1.Click += new System.EventHandler(this.getFramesRelatedToFrame1_Click);
            // 
            // getFrame1Frame2FeMapping
            // 
            this.getFrame1Frame2FeMapping.Location = new System.Drawing.Point(241, 465);
            this.getFrame1Frame2FeMapping.Name = "getFrame1Frame2FeMapping";
            this.getFrame1Frame2FeMapping.Size = new System.Drawing.Size(212, 23);
            this.getFrame1Frame2FeMapping.TabIndex = 15;
            this.getFrame1Frame2FeMapping.Text = "Get FE mapping between frames 1 and 2";
            this.getFrame1Frame2FeMapping.UseVisualStyleBackColor = true;
            this.getFrame1Frame2FeMapping.Click += new System.EventHandler(this.getFrame1Frame2FeMapping_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(27, 470);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(48, 13);
            this.label2.TabIndex = 10;
            this.label2.Text = "Frame 2:";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(24, 246);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(49, 13);
            this.label3.TabIndex = 11;
            this.label3.Text = "Relation:";
            // 
            // recursive
            // 
            this.recursive.AutoSize = true;
            this.recursive.Location = new System.Drawing.Point(81, 357);
            this.recursive.Name = "recursive";
            this.recursive.Size = new System.Drawing.Size(74, 17);
            this.recursive.TabIndex = 12;
            this.recursive.Text = "Recursive";
            this.recursive.UseVisualStyleBackColor = true;
            // 
            // numItemsLbl
            // 
            this.numItemsLbl.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.numItemsLbl.AutoSize = true;
            this.numItemsLbl.Location = new System.Drawing.Point(878, 18);
            this.numItemsLbl.Name = "numItemsLbl";
            this.numItemsLbl.Size = new System.Drawing.Size(40, 13);
            this.numItemsLbl.TabIndex = 13;
            this.numItemsLbl.Text = "0 items";
            // 
            // checkFrames
            // 
            this.checkFrames.Location = new System.Drawing.Point(81, 577);
            this.checkFrames.Name = "checkFrames";
            this.checkFrames.Size = new System.Drawing.Size(142, 23);
            this.checkFrames.TabIndex = 18;
            this.checkFrames.Text = "Check all frames";
            this.checkFrames.UseVisualStyleBackColor = true;
            this.checkFrames.Click += new System.EventHandler(this.checkFrames_Click);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(21, 336);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(52, 13);
            this.label4.TabIndex = 16;
            this.label4.Text = "Direction:";
            // 
            // lexicalUnit
            // 
            this.lexicalUnit.Location = new System.Drawing.Point(81, 36);
            this.lexicalUnit.Name = "lexicalUnit";
            this.lexicalUnit.Size = new System.Drawing.Size(117, 20);
            this.lexicalUnit.TabIndex = 0;
            // 
            // getAttestationsForLexicalUnit
            // 
            this.getAttestationsForLexicalUnit.Location = new System.Drawing.Point(277, 34);
            this.getAttestationsForLexicalUnit.Name = "getAttestationsForLexicalUnit";
            this.getAttestationsForLexicalUnit.Size = new System.Drawing.Size(90, 23);
            this.getAttestationsForLexicalUnit.TabIndex = 2;
            this.getAttestationsForLexicalUnit.Text = "Get attestations";
            this.getAttestationsForLexicalUnit.UseVisualStyleBackColor = true;
            this.getAttestationsForLexicalUnit.Click += new System.EventHandler(this.getAttestationsForLexicalUnit_Click);
            // 
            // both
            // 
            this.both.AutoSize = true;
            this.both.Location = new System.Drawing.Point(194, 334);
            this.both.Name = "both";
            this.both.Size = new System.Drawing.Size(47, 17);
            this.both.TabIndex = 11;
            this.both.Text = "Both";
            this.both.UseVisualStyleBackColor = true;
            // 
            // super
            // 
            this.super.AutoSize = true;
            this.super.Location = new System.Drawing.Point(135, 334);
            this.super.Name = "super";
            this.super.Size = new System.Drawing.Size(53, 17);
            this.super.TabIndex = 10;
            this.super.Text = "Super";
            this.super.UseVisualStyleBackColor = true;
            // 
            // sub
            // 
            this.sub.AutoSize = true;
            this.sub.Checked = true;
            this.sub.Location = new System.Drawing.Point(81, 334);
            this.sub.Name = "sub";
            this.sub.Size = new System.Drawing.Size(44, 17);
            this.sub.TabIndex = 9;
            this.sub.TabStop = true;
            this.sub.Text = "Sub";
            this.sub.UseVisualStyleBackColor = true;
            // 
            // lexicalUnitPOS
            // 
            this.lexicalUnitPOS.Location = new System.Drawing.Point(81, 62);
            this.lexicalUnitPOS.Name = "lexicalUnitPOS";
            this.lexicalUnitPOS.Size = new System.Drawing.Size(43, 20);
            this.lexicalUnitPOS.TabIndex = 3;
            // 
            // getFramesForLexicalUnit
            // 
            this.getFramesForLexicalUnit.Location = new System.Drawing.Point(204, 34);
            this.getFramesForLexicalUnit.Name = "getFramesForLexicalUnit";
            this.getFramesForLexicalUnit.Size = new System.Drawing.Size(67, 23);
            this.getFramesForLexicalUnit.TabIndex = 1;
            this.getFramesForLexicalUnit.Text = "Get frames";
            this.getFramesForLexicalUnit.UseVisualStyleBackColor = true;
            this.getFramesForLexicalUnit.Click += new System.EventHandler(this.getFramesForLexicalUnit_Click);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(12, 39);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(63, 13);
            this.label6.TabIndex = 24;
            this.label6.Text = "Lexical unit:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(43, 65);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(32, 13);
            this.label7.TabIndex = 25;
            this.label7.Text = "POS:";
            // 
            // relations
            // 
            this.relations.FormattingEnabled = true;
            this.relations.Location = new System.Drawing.Point(81, 246);
            this.relations.Name = "relations";
            this.relations.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.relations.Size = new System.Drawing.Size(156, 82);
            this.relations.Sorted = true;
            this.relations.TabIndex = 8;
            // 
            // frame1
            // 
            this.frame1.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
            this.frame1.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.frame1.DropDownHeight = 250;
            this.frame1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.frame1.FormattingEnabled = true;
            this.frame1.IntegralHeight = false;
            this.frame1.Location = new System.Drawing.Point(81, 143);
            this.frame1.Name = "frame1";
            this.frame1.Size = new System.Drawing.Size(154, 21);
            this.frame1.Sorted = true;
            this.frame1.TabIndex = 4;
            this.frame1.SelectedIndexChanged += new System.EventHandler(this.frame1_SelectedIndexChanged);
            // 
            // fe1
            // 
            this.fe1.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
            this.fe1.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.fe1.DropDownHeight = 250;
            this.fe1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.fe1.FormattingEnabled = true;
            this.fe1.IntegralHeight = false;
            this.fe1.Location = new System.Drawing.Point(81, 170);
            this.fe1.Name = "fe1";
            this.fe1.Size = new System.Drawing.Size(154, 21);
            this.fe1.Sorted = true;
            this.fe1.TabIndex = 7;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(43, 173);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(32, 13);
            this.label8.TabIndex = 29;
            this.label8.Text = "FE 1:";
            // 
            // fe2
            // 
            this.fe2.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
            this.fe2.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.fe2.DropDownHeight = 250;
            this.fe2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.fe2.FormattingEnabled = true;
            this.fe2.IntegralHeight = false;
            this.fe2.Location = new System.Drawing.Point(81, 494);
            this.fe2.Name = "fe2";
            this.fe2.Size = new System.Drawing.Size(154, 21);
            this.fe2.Sorted = true;
            this.fe2.TabIndex = 16;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(41, 497);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(32, 13);
            this.label9.TabIndex = 32;
            this.label9.Text = "FE 2:";
            // 
            // frame2
            // 
            this.frame2.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
            this.frame2.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
            this.frame2.DropDownHeight = 250;
            this.frame2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.frame2.FormattingEnabled = true;
            this.frame2.IntegralHeight = false;
            this.frame2.Location = new System.Drawing.Point(81, 467);
            this.frame2.Name = "frame2";
            this.frame2.Size = new System.Drawing.Size(154, 21);
            this.frame2.Sorted = true;
            this.frame2.TabIndex = 14;
            this.frame2.SelectedIndexChanged += new System.EventHandler(this.frame2_SelectedIndexChanged);
            // 
            // getFe1Fe2Path
            // 
            this.getFe1Fe2Path.Location = new System.Drawing.Point(241, 492);
            this.getFe1Fe2Path.Name = "getFe1Fe2Path";
            this.getFe1Fe2Path.Size = new System.Drawing.Size(212, 23);
            this.getFe1Fe2Path.TabIndex = 17;
            this.getFe1Fe2Path.Text = "Get path from FE 1 to FE 2";
            this.getFe1Fe2Path.UseVisualStyleBackColor = true;
            this.getFe1Fe2Path.Click += new System.EventHandler(this.getFe1Fe2Path_Click);
            // 
            // output
            // 
            this.output.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.output.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.output.Location = new System.Drawing.Point(476, 34);
            this.output.Multiline = true;
            this.output.Name = "output";
            this.output.ReadOnly = true;
            this.output.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output.Size = new System.Drawing.Size(504, 566);
            this.output.TabIndex = 19;
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(992, 622);
            this.Controls.Add(this.output);
            this.Controls.Add(this.getFe1Fe2Path);
            this.Controls.Add(this.fe2);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.frame2);
            this.Controls.Add(this.fe1);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.frame1);
            this.Controls.Add(this.relations);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.getFramesForLexicalUnit);
            this.Controls.Add(this.lexicalUnitPOS);
            this.Controls.Add(this.lexicalUnit);
            this.Controls.Add(this.getAttestationsForLexicalUnit);
            this.Controls.Add(this.numItemsLbl);
            this.Controls.Add(this.both);
            this.Controls.Add(this.getLexicalUnitsForFrame1);
            this.Controls.Add(this.checkFrames);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.getFrame1Frame2FeMapping);
            this.Controls.Add(this.recursive);
            this.Controls.Add(this.super);
            this.Controls.Add(this.getAttestationsForFrame1);
            this.Controls.Add(this.sub);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.getFramesRelatedToFrame1);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Name = "TestForm";
            this.Text = "Test Application";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button getAttestationsForFrame1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button getLexicalUnitsForFrame1;
        private System.Windows.Forms.Button getFramesRelatedToFrame1;
        private System.Windows.Forms.Button getFrame1Frame2FeMapping;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.CheckBox recursive;
        private System.Windows.Forms.Label numItemsLbl;
        private System.Windows.Forms.Button checkFrames;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.RadioButton super;
        private System.Windows.Forms.RadioButton sub;
        private System.Windows.Forms.RadioButton both;
        private System.Windows.Forms.Button getAttestationsForLexicalUnit;
        private System.Windows.Forms.TextBox lexicalUnit;
        private System.Windows.Forms.TextBox lexicalUnitPOS;
        private System.Windows.Forms.Button getFramesForLexicalUnit;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.ListBox relations;
        private System.Windows.Forms.ComboBox frame1;
        private System.Windows.Forms.ComboBox fe1;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.ComboBox fe2;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.ComboBox frame2;
        private System.Windows.Forms.Button getFe1Fe2Path;
        private System.Windows.Forms.TextBox output;
    }
}

