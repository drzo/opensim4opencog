namespace cogbot.DotCYC
{
    partial class CycConnectionForm
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
            this.btnConnect = new System.Windows.Forms.Button();
            this.cycServerAddress = new System.Windows.Forms.TextBox();
            this.txtCycOutput = new System.Windows.Forms.TextBox();
            this.txtEvalString = new System.Windows.Forms.TextBox();
            this.btnEval = new System.Windows.Forms.Button();
            this.cycBasePort = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // btnConnect
            // 
            this.btnConnect.Location = new System.Drawing.Point(158, 12);
            this.btnConnect.Name = "btnConnect";
            this.btnConnect.Size = new System.Drawing.Size(84, 22);
            this.btnConnect.TabIndex = 0;
            this.btnConnect.Text = "Connect";
            this.btnConnect.UseVisualStyleBackColor = true;
            this.btnConnect.Click += new System.EventHandler(this.button1_Click);
            // 
            // cycServerAddress
            // 
            this.cycServerAddress.Location = new System.Drawing.Point(12, 12);
            this.cycServerAddress.Name = "cycServerAddress";
            this.cycServerAddress.Size = new System.Drawing.Size(83, 20);
            this.cycServerAddress.TabIndex = 1;
            this.cycServerAddress.Text = "CycServer";
            // 
            // txtCycOutput
            // 
            this.txtCycOutput.Location = new System.Drawing.Point(12, 58);
            this.txtCycOutput.Multiline = true;
            this.txtCycOutput.Name = "txtCycOutput";
            this.txtCycOutput.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.txtCycOutput.Size = new System.Drawing.Size(235, 109);
            this.txtCycOutput.TabIndex = 2;
            this.txtCycOutput.Text = "Set the CycServer to the IP of your Instance\r\nor edit \r\nC:\\WINDOWS\\system32\\drive" +
                "rs\\etc\\hosts\r\nAdding a lines like:\r\n10.10.10.198   CycServer\r\n10.10.10.197   Ope" +
                "nSimServer";
            // 
            // txtEvalString
            // 
            this.txtEvalString.Location = new System.Drawing.Point(12, 193);
            this.txtEvalString.Name = "txtEvalString";
            this.txtEvalString.Size = new System.Drawing.Size(235, 20);
            this.txtEvalString.TabIndex = 3;
            this.txtEvalString.Text = "(cyc-query \'(#$isa ?X #$Dog) #$EverythingPSC)";
            // 
            // btnEval
            // 
            this.btnEval.Location = new System.Drawing.Point(189, 219);
            this.btnEval.Name = "btnEval";
            this.btnEval.Size = new System.Drawing.Size(53, 22);
            this.btnEval.TabIndex = 4;
            this.btnEval.Text = "EVAL";
            this.btnEval.UseVisualStyleBackColor = true;
            this.btnEval.Click += new System.EventHandler(this.btnEval_Click);
            // 
            // cycBasePort
            // 
            this.cycBasePort.Location = new System.Drawing.Point(101, 12);
            this.cycBasePort.Name = "cycBasePort";
            this.cycBasePort.Size = new System.Drawing.Size(51, 20);
            this.cycBasePort.TabIndex = 5;
            this.cycBasePort.Text = "3600";
            // 
            // CycConnectionForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(254, 252);
            this.Controls.Add(this.cycBasePort);
            this.Controls.Add(this.btnEval);
            this.Controls.Add(this.txtEvalString);
            this.Controls.Add(this.txtCycOutput);
            this.Controls.Add(this.cycServerAddress);
            this.Controls.Add(this.btnConnect);
            this.Name = "CycConnectionForm";
            this.Text = "CycConnectionForm";
            this.Load += new System.EventHandler(this.CycConnectionForm_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button btnConnect;
        private System.Windows.Forms.TextBox cycServerAddress;
        private System.Windows.Forms.TextBox txtCycOutput;
        private System.Windows.Forms.TextBox txtEvalString;
        private System.Windows.Forms.Button btnEval;
        private System.Windows.Forms.TextBox cycBasePort;
    }
}