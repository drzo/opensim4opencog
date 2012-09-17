namespace hillpeople2d
{
    partial class mainwindow
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
            this.botarea = new System.Windows.Forms.Panel();
            this.playfieldcontainer = new System.Windows.Forms.Panel();
            this.navpanel = new System.Windows.Forms.Panel();
            this.botarea.SuspendLayout();
            this.SuspendLayout();
            // 
            // botarea
            // 
            this.botarea.AutoSize = true;
            this.botarea.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.botarea.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.botarea.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.botarea.Controls.Add(this.navpanel);
            this.botarea.Dock = System.Windows.Forms.DockStyle.Left;
            this.botarea.Location = new System.Drawing.Point(0, 0);
            this.botarea.Margin = new System.Windows.Forms.Padding(0);
            this.botarea.MinimumSize = new System.Drawing.Size(129, 2);
            this.botarea.Name = "botarea";
            this.botarea.Size = new System.Drawing.Size(129, 355);
            this.botarea.TabIndex = 0;
            // 
            // playfieldcontainer
            // 
            this.playfieldcontainer.AutoScroll = true;
            this.playfieldcontainer.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(57)))), ((int)(((byte)(181)))), ((int)(((byte)(74)))));
            this.playfieldcontainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.playfieldcontainer.Location = new System.Drawing.Point(129, 0);
            this.playfieldcontainer.Margin = new System.Windows.Forms.Padding(0);
            this.playfieldcontainer.Name = "playfieldcontainer";
            this.playfieldcontainer.Size = new System.Drawing.Size(720, 355);
            this.playfieldcontainer.TabIndex = 1;
            this.playfieldcontainer.Paint += new System.Windows.Forms.PaintEventHandler(this.playfieldcontainer_Paint);
            this.playfieldcontainer.MouseDown += new System.Windows.Forms.MouseEventHandler(this.playfieldcontainer_MouseDown);
            this.playfieldcontainer.MouseUp += new System.Windows.Forms.MouseEventHandler(this.playfieldcontainer_MouseUp);
            // 
            // navpanel
            // 
            this.navpanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.navpanel.Location = new System.Drawing.Point(0, 0);
            this.navpanel.Margin = new System.Windows.Forms.Padding(0);
            this.navpanel.Name = "navpanel";
            this.navpanel.Size = new System.Drawing.Size(127, 72);
            this.navpanel.TabIndex = 0;
            this.navpanel.Paint += new System.Windows.Forms.PaintEventHandler(this.navpanel_Paint);
            this.navpanel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.navpanel_MouseUp);
            // 
            // mainwindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(849, 355);
            this.Controls.Add(this.playfieldcontainer);
            this.Controls.Add(this.botarea);
            this.Name = "mainwindow";
            this.Text = "Hill People 2D";
            this.botarea.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Panel botarea;
        private System.Windows.Forms.Panel playfieldcontainer;
        private System.Windows.Forms.Panel navpanel;
    }
}

