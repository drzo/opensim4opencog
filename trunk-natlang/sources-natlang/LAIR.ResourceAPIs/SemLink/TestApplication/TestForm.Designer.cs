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
            this.verb = new System.Windows.Forms.TextBox();
            this.roleSet = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.verbRole = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.vnClass = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.vnRole = new System.Windows.Forms.TextBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.frame = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.frameElement = new System.Windows.Forms.TextBox();
            this.output = new System.Windows.Forms.TextBox();
            this.linkPbVn = new System.Windows.Forms.Button();
            this.linkVnFn = new System.Windows.Forms.Button();
            this.linkPbFn = new System.Windows.Forms.Button();
            this.super = new System.Windows.Forms.RadioButton();
            this.both = new System.Windows.Forms.RadioButton();
            this.sub = new System.Windows.Forms.RadioButton();
            this.none = new System.Windows.Forms.RadioButton();
            this.label8 = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(27, 22);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(32, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Verb:";
            // 
            // verb
            // 
            this.verb.Location = new System.Drawing.Point(65, 19);
            this.verb.Name = "verb";
            this.verb.Size = new System.Drawing.Size(91, 20);
            this.verb.TabIndex = 0;
            // 
            // roleSet
            // 
            this.roleSet.Location = new System.Drawing.Point(65, 45);
            this.roleSet.Name = "roleSet";
            this.roleSet.Size = new System.Drawing.Size(91, 20);
            this.roleSet.TabIndex = 1;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(10, 48);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(49, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Role set:";
            // 
            // verbRole
            // 
            this.verbRole.Location = new System.Drawing.Point(65, 71);
            this.verbRole.Name = "verbRole";
            this.verbRole.Size = new System.Drawing.Size(91, 20);
            this.verbRole.TabIndex = 2;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(27, 74);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(32, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Role:";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.verb);
            this.groupBox1.Controls.Add(this.verbRole);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.label3);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.roleSet);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(170, 100);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "PropBank";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.vnClass);
            this.groupBox2.Controls.Add(this.label4);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.vnRole);
            this.groupBox2.Location = new System.Drawing.Point(188, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(170, 100);
            this.groupBox2.TabIndex = 1;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "VerbNet";
            // 
            // vnClass
            // 
            this.vnClass.Location = new System.Drawing.Point(58, 19);
            this.vnClass.Name = "vnClass";
            this.vnClass.Size = new System.Drawing.Size(91, 20);
            this.vnClass.TabIndex = 0;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(17, 22);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(35, 13);
            this.label4.TabIndex = 0;
            this.label4.Text = "Class:";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(20, 48);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(32, 13);
            this.label6.TabIndex = 2;
            this.label6.Text = "Role:";
            // 
            // vnRole
            // 
            this.vnRole.Location = new System.Drawing.Point(58, 45);
            this.vnRole.Name = "vnRole";
            this.vnRole.Size = new System.Drawing.Size(91, 20);
            this.vnRole.TabIndex = 1;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.frame);
            this.groupBox3.Controls.Add(this.label5);
            this.groupBox3.Controls.Add(this.label7);
            this.groupBox3.Controls.Add(this.frameElement);
            this.groupBox3.Location = new System.Drawing.Point(364, 12);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(170, 100);
            this.groupBox3.TabIndex = 2;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "FrameNet";
            // 
            // frame
            // 
            this.frame.Location = new System.Drawing.Point(61, 19);
            this.frame.Name = "frame";
            this.frame.Size = new System.Drawing.Size(91, 20);
            this.frame.TabIndex = 0;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(16, 22);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(39, 13);
            this.label5.TabIndex = 0;
            this.label5.Text = "Frame:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(32, 48);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(23, 13);
            this.label7.TabIndex = 2;
            this.label7.Text = "FE:";
            // 
            // frameElement
            // 
            this.frameElement.Location = new System.Drawing.Point(61, 45);
            this.frameElement.Name = "frameElement";
            this.frameElement.Size = new System.Drawing.Size(91, 20);
            this.frameElement.TabIndex = 1;
            // 
            // output
            // 
            this.output.Font = new System.Drawing.Font("Courier New", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.output.Location = new System.Drawing.Point(13, 199);
            this.output.Multiline = true;
            this.output.Name = "output";
            this.output.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.output.Size = new System.Drawing.Size(521, 219);
            this.output.TabIndex = 6;
            // 
            // linkPbVn
            // 
            this.linkPbVn.Location = new System.Drawing.Point(155, 118);
            this.linkPbVn.Name = "linkPbVn";
            this.linkPbVn.Size = new System.Drawing.Size(60, 23);
            this.linkPbVn.TabIndex = 3;
            this.linkPbVn.Text = "<- Link ->";
            this.linkPbVn.UseVisualStyleBackColor = true;
            this.linkPbVn.Click += new System.EventHandler(this.linkPbVn_Click);
            // 
            // linkVnFn
            // 
            this.linkVnFn.Location = new System.Drawing.Point(330, 118);
            this.linkVnFn.Name = "linkVnFn";
            this.linkVnFn.Size = new System.Drawing.Size(60, 23);
            this.linkVnFn.TabIndex = 5;
            this.linkVnFn.Text = "<- Link ->";
            this.linkVnFn.UseVisualStyleBackColor = true;
            this.linkVnFn.Click += new System.EventHandler(this.linkVnFn_Click);
            // 
            // linkPbFn
            // 
            this.linkPbFn.Location = new System.Drawing.Point(155, 147);
            this.linkPbFn.Name = "linkPbFn";
            this.linkPbFn.Size = new System.Drawing.Size(235, 23);
            this.linkPbFn.TabIndex = 4;
            this.linkPbFn.Text = "<- Link ->";
            this.linkPbFn.UseVisualStyleBackColor = true;
            this.linkPbFn.Click += new System.EventHandler(this.linkPbFn_Click);
            // 
            // super
            // 
            this.super.AutoSize = true;
            this.super.Location = new System.Drawing.Point(252, 176);
            this.super.Name = "super";
            this.super.Size = new System.Drawing.Size(53, 17);
            this.super.TabIndex = 7;
            this.super.Text = "Super";
            this.super.UseVisualStyleBackColor = true;
            // 
            // both
            // 
            this.both.AutoSize = true;
            this.both.Location = new System.Drawing.Point(361, 176);
            this.both.Name = "both";
            this.both.Size = new System.Drawing.Size(47, 17);
            this.both.TabIndex = 8;
            this.both.Text = "Both";
            this.both.UseVisualStyleBackColor = true;
            // 
            // sub
            // 
            this.sub.AutoSize = true;
            this.sub.Location = new System.Drawing.Point(311, 176);
            this.sub.Name = "sub";
            this.sub.Size = new System.Drawing.Size(44, 17);
            this.sub.TabIndex = 9;
            this.sub.Text = "Sub";
            this.sub.UseVisualStyleBackColor = true;
            // 
            // none
            // 
            this.none.AutoSize = true;
            this.none.Checked = true;
            this.none.Location = new System.Drawing.Point(195, 176);
            this.none.Name = "none";
            this.none.Size = new System.Drawing.Size(51, 17);
            this.none.TabIndex = 10;
            this.none.TabStop = true;
            this.none.Text = "None";
            this.none.UseVisualStyleBackColor = true;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(133, 178);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(56, 13);
            this.label8.TabIndex = 11;
            this.label8.Text = "Extension:";
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(545, 430);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.none);
            this.Controls.Add(this.sub);
            this.Controls.Add(this.both);
            this.Controls.Add(this.super);
            this.Controls.Add(this.linkPbFn);
            this.Controls.Add(this.linkVnFn);
            this.Controls.Add(this.linkPbVn);
            this.Controls.Add(this.output);
            this.Controls.Add(this.groupBox3);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "TestForm";
            this.Text = "SemLink test application";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox verb;
        private System.Windows.Forms.TextBox roleSet;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox verbRole;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox vnClass;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox vnRole;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.TextBox frame;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox frameElement;
        private System.Windows.Forms.TextBox output;
        private System.Windows.Forms.Button linkPbVn;
        private System.Windows.Forms.Button linkVnFn;
        private System.Windows.Forms.Button linkPbFn;
        private System.Windows.Forms.RadioButton super;
        private System.Windows.Forms.RadioButton both;
        private System.Windows.Forms.RadioButton sub;
        private System.Windows.Forms.RadioButton none;
        private System.Windows.Forms.Label label8;
    }
}

