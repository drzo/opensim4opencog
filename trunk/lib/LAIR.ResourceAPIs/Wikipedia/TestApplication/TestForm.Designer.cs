namespace TestProject
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
            this.titleBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.getPageBtn = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.linkList = new System.Windows.Forms.ListBox();
            this.linkLbl = new System.Windows.Forms.Label();
            this.headingTree = new System.Windows.Forms.TreeView();
            this.markupBox = new System.Windows.Forms.TextBox();
            this.mainRadio = new System.Windows.Forms.RadioButton();
            this.auxBtn = new System.Windows.Forms.RadioButton();
            this.showSecLayoutBtn = new System.Windows.Forms.Button();
            this.checkConsistBtn = new System.Windows.Forms.Button();
            this.speedTestBtn = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.resultLbl = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // titleBox
            // 
            this.titleBox.Location = new System.Drawing.Point(72, 25);
            this.titleBox.Name = "titleBox";
            this.titleBox.Size = new System.Drawing.Size(225, 20);
            this.titleBox.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(54, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Page title:";
            // 
            // getPageBtn
            // 
            this.getPageBtn.Location = new System.Drawing.Point(303, 23);
            this.getPageBtn.Name = "getPageBtn";
            this.getPageBtn.Size = new System.Drawing.Size(115, 23);
            this.getPageBtn.TabIndex = 2;
            this.getPageBtn.Text = "Get Page";
            this.getPageBtn.UseVisualStyleBackColor = true;
            this.getPageBtn.Click += new System.EventHandler(this.getPageBtn_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(9, 105);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(85, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Heading Layout:";
            // 
            // linkList
            // 
            this.linkList.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.linkList.FormattingEnabled = true;
            this.linkList.ItemHeight = 14;
            this.linkList.Location = new System.Drawing.Point(414, 121);
            this.linkList.Name = "linkList";
            this.linkList.Size = new System.Drawing.Size(575, 452);
            this.linkList.TabIndex = 5;
            this.linkList.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.linkList_MouseDoubleClick);
            // 
            // linkLbl
            // 
            this.linkLbl.AutoSize = true;
            this.linkLbl.Location = new System.Drawing.Point(411, 105);
            this.linkLbl.Name = "linkLbl";
            this.linkLbl.Size = new System.Drawing.Size(59, 13);
            this.linkLbl.TabIndex = 6;
            this.linkLbl.Text = "Wiki Links:";
            // 
            // headingTree
            // 
            this.headingTree.Font = new System.Drawing.Font("Times New Roman", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.headingTree.Location = new System.Drawing.Point(12, 121);
            this.headingTree.Name = "headingTree";
            this.headingTree.Size = new System.Drawing.Size(396, 452);
            this.headingTree.TabIndex = 11;
            this.headingTree.DoubleClick += new System.EventHandler(this.headingTree_DoubleClick);
            // 
            // markupBox
            // 
            this.markupBox.Location = new System.Drawing.Point(130, 582);
            this.markupBox.Name = "markupBox";
            this.markupBox.Size = new System.Drawing.Size(278, 20);
            this.markupBox.TabIndex = 13;
            this.markupBox.TextChanged += new System.EventHandler(this.markupBox_TextChanged);
            // 
            // mainRadio
            // 
            this.mainRadio.AutoSize = true;
            this.mainRadio.Checked = true;
            this.mainRadio.Location = new System.Drawing.Point(559, 29);
            this.mainRadio.Name = "mainRadio";
            this.mainRadio.Size = new System.Drawing.Size(48, 17);
            this.mainRadio.TabIndex = 17;
            this.mainRadio.TabStop = true;
            this.mainRadio.Text = "Main";
            this.mainRadio.UseVisualStyleBackColor = true;
            // 
            // auxBtn
            // 
            this.auxBtn.AutoSize = true;
            this.auxBtn.Location = new System.Drawing.Point(559, 52);
            this.auxBtn.Name = "auxBtn";
            this.auxBtn.Size = new System.Drawing.Size(63, 17);
            this.auxBtn.TabIndex = 18;
            this.auxBtn.Text = "Auxiliary";
            this.auxBtn.UseVisualStyleBackColor = true;
            // 
            // showSecLayoutBtn
            // 
            this.showSecLayoutBtn.Location = new System.Drawing.Point(303, 52);
            this.showSecLayoutBtn.Name = "showSecLayoutBtn";
            this.showSecLayoutBtn.Size = new System.Drawing.Size(115, 23);
            this.showSecLayoutBtn.TabIndex = 19;
            this.showSecLayoutBtn.Text = "Show Layout";
            this.showSecLayoutBtn.UseVisualStyleBackColor = true;
            this.showSecLayoutBtn.Click += new System.EventHandler(this.showSecLayoutBtn_Click);
            // 
            // checkConsistBtn
            // 
            this.checkConsistBtn.Location = new System.Drawing.Point(424, 23);
            this.checkConsistBtn.Name = "checkConsistBtn";
            this.checkConsistBtn.Size = new System.Drawing.Size(115, 23);
            this.checkConsistBtn.TabIndex = 20;
            this.checkConsistBtn.Text = "Check Mirror";
            this.checkConsistBtn.UseVisualStyleBackColor = true;
            this.checkConsistBtn.Click += new System.EventHandler(this.checkConsistBtn_Click);
            // 
            // speedTestBtn
            // 
            this.speedTestBtn.Location = new System.Drawing.Point(424, 52);
            this.speedTestBtn.Name = "speedTestBtn";
            this.speedTestBtn.Size = new System.Drawing.Size(115, 23);
            this.speedTestBtn.TabIndex = 21;
            this.speedTestBtn.Text = "Speed Test";
            this.speedTestBtn.UseVisualStyleBackColor = true;
            this.speedTestBtn.Click += new System.EventHandler(this.speedTestBtn_Click);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(9, 585);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(115, 13);
            this.label3.TabIndex = 22;
            this.label3.Text = "Test Markup Removal:";
            // 
            // resultLbl
            // 
            this.resultLbl.AutoSize = true;
            this.resultLbl.Location = new System.Drawing.Point(84, 613);
            this.resultLbl.Name = "resultLbl";
            this.resultLbl.Size = new System.Drawing.Size(40, 13);
            this.resultLbl.TabIndex = 23;
            this.resultLbl.Text = "Result:";
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1006, 648);
            this.Controls.Add(this.resultLbl);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.speedTestBtn);
            this.Controls.Add(this.checkConsistBtn);
            this.Controls.Add(this.showSecLayoutBtn);
            this.Controls.Add(this.auxBtn);
            this.Controls.Add(this.mainRadio);
            this.Controls.Add(this.markupBox);
            this.Controls.Add(this.headingTree);
            this.Controls.Add(this.linkLbl);
            this.Controls.Add(this.linkList);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.getPageBtn);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.titleBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.MaximizeBox = false;
            this.Name = "TestForm";
            this.Text = "Wikipedia.Net Test";
            this.Load += new System.EventHandler(this.TestForm_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox titleBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button getPageBtn;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ListBox linkList;
        private System.Windows.Forms.Label linkLbl;
        private System.Windows.Forms.TreeView headingTree;
        private System.Windows.Forms.TextBox markupBox;
        private System.Windows.Forms.RadioButton mainRadio;
        private System.Windows.Forms.RadioButton auxBtn;
        private System.Windows.Forms.Button showSecLayoutBtn;
        private System.Windows.Forms.Button checkConsistBtn;
        private System.Windows.Forms.Button speedTestBtn;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label resultLbl;
    }
}

