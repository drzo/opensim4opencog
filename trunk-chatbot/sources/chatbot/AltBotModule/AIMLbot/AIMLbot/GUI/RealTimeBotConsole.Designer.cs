using System.Windows.Forms;

namespace AltAIMLbot.GUI
{
    partial class AIMLPadEditor 
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
            this.splitContainer4 = new System.Windows.Forms.SplitContainer();
            this.splitContainer5 = new System.Windows.Forms.SplitContainer();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.clientToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.loginToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.logoutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.simbotStopToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.simbotThinkToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.simbotOffToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.splitContainer6 = new System.Windows.Forms.SplitContainer();
            this.label1 = new System.Windows.Forms.Label();
            this.robotNameBox = new System.Windows.Forms.ComboBox();
            this.splitContainer7 = new System.Windows.Forms.SplitContainer();
            this.userNameBox = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.dictionaryNameBox = new System.Windows.Forms.ComboBox();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.consoleText = new System.Windows.Forms.TextBox();
            this.consoleInputText = new System.Windows.Forms.ComboBox();
            this.thatInputBox = new System.Windows.Forms.ComboBox();
            this.splitContainer3 = new System.Windows.Forms.SplitContainer();
            this.variablesOutput = new System.Windows.Forms.TextBox();
            this.graphNameBox = new System.Windows.Forms.ComboBox();
            this.submitButton = new System.Windows.Forms.Button();
            this.topicInputBox = new System.Windows.Forms.ComboBox();
            this.splitContainer4.Panel1.SuspendLayout();
            this.splitContainer4.Panel2.SuspendLayout();
            this.splitContainer4.SuspendLayout();
            this.splitContainer5.Panel1.SuspendLayout();
            this.splitContainer5.Panel2.SuspendLayout();
            this.splitContainer5.SuspendLayout();
            this.menuStrip1.SuspendLayout();
            this.splitContainer6.Panel1.SuspendLayout();
            this.splitContainer6.Panel2.SuspendLayout();
            this.splitContainer6.SuspendLayout();
            this.splitContainer7.Panel1.SuspendLayout();
            this.splitContainer7.Panel2.SuspendLayout();
            this.splitContainer7.SuspendLayout();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            this.splitContainer3.Panel1.SuspendLayout();
            this.splitContainer3.Panel2.SuspendLayout();
            this.splitContainer3.SuspendLayout();
            this.SuspendLayout();
            // 
            // splitContainer4
            // 
            this.splitContainer4.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer4.Location = new System.Drawing.Point(0, 0);
            this.splitContainer4.Name = "splitContainer4";
            this.splitContainer4.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer4.Panel1
            // 
            this.splitContainer4.Panel1.Controls.Add(this.splitContainer5);
            // 
            // splitContainer4.Panel2
            // 
            this.splitContainer4.Panel2.Controls.Add(this.splitContainer1);
            this.splitContainer4.Size = new System.Drawing.Size(824, 633);
            this.splitContainer4.SplitterDistance = 51;
            this.splitContainer4.TabIndex = 20;
            // 
            // splitContainer5
            // 
            this.splitContainer5.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer5.Location = new System.Drawing.Point(0, 0);
            this.splitContainer5.Name = "splitContainer5";
            this.splitContainer5.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer5.Panel1
            // 
            this.splitContainer5.Panel1.Controls.Add(this.menuStrip1);
            // 
            // splitContainer5.Panel2
            // 
            this.splitContainer5.Panel2.Controls.Add(this.splitContainer6);
            this.splitContainer5.Size = new System.Drawing.Size(824, 51);
            this.splitContainer5.SplitterDistance = 25;
            this.splitContainer5.TabIndex = 0;
            // 
            // menuStrip1
            // 
            this.menuStrip1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.clientToolStripMenuItem,
            this.toolsToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(824, 25);
            this.menuStrip1.TabIndex = 2;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // clientToolStripMenuItem
            // 
            this.clientToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.loginToolStripMenuItem,
            this.logoutToolStripMenuItem,
            this.simbotStopToolStripMenuItem,
            this.simbotThinkToolStripMenuItem,
            this.simbotOffToolStripMenuItem});
            this.clientToolStripMenuItem.Name = "clientToolStripMenuItem";
            this.clientToolStripMenuItem.Size = new System.Drawing.Size(51, 21);
            this.clientToolStripMenuItem.Text = "SimBot";
            // 
            // loginToolStripMenuItem
            // 
            this.loginToolStripMenuItem.Name = "loginToolStripMenuItem";
            this.loginToolStripMenuItem.Size = new System.Drawing.Size(131, 22);
            this.loginToolStripMenuItem.Text = "simbot info";
            // 
            // logoutToolStripMenuItem
            // 
            this.logoutToolStripMenuItem.Name = "logoutToolStripMenuItem";
            this.logoutToolStripMenuItem.Size = new System.Drawing.Size(131, 22);
            this.logoutToolStripMenuItem.Text = "simbot start";
            // 
            // simbotStopToolStripMenuItem
            // 
            this.simbotStopToolStripMenuItem.Name = "simbotStopToolStripMenuItem";
            this.simbotStopToolStripMenuItem.Size = new System.Drawing.Size(131, 22);
            this.simbotStopToolStripMenuItem.Text = "simbot stop";
            // 
            // simbotThinkToolStripMenuItem
            // 
            this.simbotThinkToolStripMenuItem.Name = "simbotThinkToolStripMenuItem";
            this.simbotThinkToolStripMenuItem.Size = new System.Drawing.Size(131, 22);
            this.simbotThinkToolStripMenuItem.Text = "simbot think";
            // 
            // simbotOffToolStripMenuItem
            // 
            this.simbotOffToolStripMenuItem.Name = "simbotOffToolStripMenuItem";
            this.simbotOffToolStripMenuItem.Size = new System.Drawing.Size(131, 22);
            this.simbotOffToolStripMenuItem.Text = "simbot off";
            // 
            // toolsToolStripMenuItem
            // 
            this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
            this.toolsToolStripMenuItem.Size = new System.Drawing.Size(44, 21);
            this.toolsToolStripMenuItem.Text = "Tools";
            // 
            // splitContainer6
            // 
            this.splitContainer6.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer6.Location = new System.Drawing.Point(0, 0);
            this.splitContainer6.Name = "splitContainer6";
            // 
            // splitContainer6.Panel1
            // 
            this.splitContainer6.Panel1.Controls.Add(this.label1);
            this.splitContainer6.Panel1.Controls.Add(this.robotNameBox);
            // 
            // splitContainer6.Panel2
            // 
            this.splitContainer6.Panel2.Controls.Add(this.splitContainer7);
            this.splitContainer6.Size = new System.Drawing.Size(824, 25);
            this.splitContainer6.SplitterDistance = 241;
            this.splitContainer6.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Dock = System.Windows.Forms.DockStyle.Left;
            this.label1.Location = new System.Drawing.Point(0, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(36, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Robot";
            this.label1.TextAlign = System.Drawing.ContentAlignment.BottomCenter;
            // 
            // robotNameBox
            // 
            this.robotNameBox.Dock = System.Windows.Forms.DockStyle.Right;
            this.robotNameBox.FormattingEnabled = true;
            this.robotNameBox.Location = new System.Drawing.Point(42, 0);
            this.robotNameBox.Name = "robotNameBox";
            this.robotNameBox.Size = new System.Drawing.Size(199, 21);
            this.robotNameBox.TabIndex = 0;
            this.robotNameBox.SelectedIndexChanged += new System.EventHandler(this.robotNameBox_SelectedIndexChanged);
            // 
            // splitContainer7
            // 
            this.splitContainer7.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer7.Location = new System.Drawing.Point(0, 0);
            this.splitContainer7.Name = "splitContainer7";
            // 
            // splitContainer7.Panel1
            // 
            this.splitContainer7.Panel1.Controls.Add(this.userNameBox);
            this.splitContainer7.Panel1.Controls.Add(this.label2);
            // 
            // splitContainer7.Panel2
            // 
            this.splitContainer7.Panel2.Controls.Add(this.dictionaryNameBox);
            this.splitContainer7.Size = new System.Drawing.Size(579, 25);
            this.splitContainer7.SplitterDistance = 193;
            this.splitContainer7.TabIndex = 0;
            // 
            // userNameBox
            // 
            this.userNameBox.FormattingEnabled = true;
            this.userNameBox.Location = new System.Drawing.Point(39, 0);
            this.userNameBox.Name = "userNameBox";
            this.userNameBox.Size = new System.Drawing.Size(153, 21);
            this.userNameBox.TabIndex = 1;
            this.userNameBox.SelectedIndexChanged += new System.EventHandler(this.userNameBox_SelectedIndexChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(4, 3);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(29, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "User";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // dictionaryNameBox
            // 
            this.dictionaryNameBox.Dock = System.Windows.Forms.DockStyle.Right;
            this.dictionaryNameBox.FormattingEnabled = true;
            this.dictionaryNameBox.Items.AddRange(new object[] {
            "@bot",
            "@set",
            "@user"});
            this.dictionaryNameBox.Location = new System.Drawing.Point(261, 0);
            this.dictionaryNameBox.Name = "dictionaryNameBox";
            this.dictionaryNameBox.Size = new System.Drawing.Size(121, 21);
            this.dictionaryNameBox.TabIndex = 0;
            this.dictionaryNameBox.Text = "@user";
            this.dictionaryNameBox.SelectedIndexChanged += new System.EventHandler(this.dictionaryNameBox_SelectedIndexChanged);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.splitContainer2);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.splitContainer3);
            this.splitContainer1.Size = new System.Drawing.Size(824, 578);
            this.splitContainer1.SplitterDistance = 597;
            this.splitContainer1.TabIndex = 20;
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            this.splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.consoleText);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.consoleInputText);
            this.splitContainer2.Panel2.Controls.Add(this.thatInputBox);
            this.splitContainer2.Size = new System.Drawing.Size(597, 578);
            this.splitContainer2.SplitterDistance = 522;
            this.splitContainer2.TabIndex = 19;
            // 
            // consoleText
            // 
            this.consoleText.BackColor = System.Drawing.SystemColors.Window;
            this.consoleText.Dock = System.Windows.Forms.DockStyle.Fill;
            this.consoleText.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.consoleText.Location = new System.Drawing.Point(0, 0);
            this.consoleText.Multiline = true;
            this.consoleText.Name = "consoleText";
            this.consoleText.ReadOnly = true;
            this.consoleText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.consoleText.Size = new System.Drawing.Size(597, 522);
            this.consoleText.TabIndex = 13;
            this.consoleText.TextChanged += new System.EventHandler(this.consoleText_TextChanged);
            // 
            // consoleInputText
            // 
            this.consoleInputText.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.consoleInputText.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.consoleInputText.FormattingEnabled = true;
            this.consoleInputText.Location = new System.Drawing.Point(0, 31);
            this.consoleInputText.Name = "consoleInputText";
            this.consoleInputText.Size = new System.Drawing.Size(597, 21);
            this.consoleInputText.TabIndex = 20;
            this.consoleInputText.SelectedIndexChanged += new System.EventHandler(this.consoleInputText_SelectedIndexChanged);
            this.consoleInputText.KeyUp += new System.Windows.Forms.KeyEventHandler(this.consoleInputText_KeyUp);
            // 
            // thatInputBox
            // 
            this.thatInputBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.thatInputBox.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.thatInputBox.FormattingEnabled = true;
            this.thatInputBox.Location = new System.Drawing.Point(0, 0);
            this.thatInputBox.Name = "thatInputBox";
            this.thatInputBox.Size = new System.Drawing.Size(597, 21);
            this.thatInputBox.TabIndex = 17;
            this.thatInputBox.SelectedIndexChanged += new System.EventHandler(this.thatInputBox_SelectedIndexChanged);
            this.thatInputBox.KeyUp += new System.Windows.Forms.KeyEventHandler(this.thatInputBox_KeyUp);
            // 
            // splitContainer3
            // 
            this.splitContainer3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer3.Location = new System.Drawing.Point(0, 0);
            this.splitContainer3.Name = "splitContainer3";
            this.splitContainer3.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer3.Panel1
            // 
            this.splitContainer3.Panel1.Controls.Add(this.variablesOutput);
            // 
            // splitContainer3.Panel2
            // 
            this.splitContainer3.Panel2.Controls.Add(this.graphNameBox);
            this.splitContainer3.Panel2.Controls.Add(this.submitButton);
            this.splitContainer3.Panel2.Controls.Add(this.topicInputBox);
            this.splitContainer3.Size = new System.Drawing.Size(223, 578);
            this.splitContainer3.SplitterDistance = 522;
            this.splitContainer3.TabIndex = 17;
            // 
            // variablesOutput
            // 
            this.variablesOutput.BackColor = System.Drawing.SystemColors.Window;
            this.variablesOutput.Dock = System.Windows.Forms.DockStyle.Fill;
            this.variablesOutput.Font = new System.Drawing.Font("Verdana", 6.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.variablesOutput.Location = new System.Drawing.Point(0, 0);
            this.variablesOutput.Multiline = true;
            this.variablesOutput.Name = "variablesOutput";
            this.variablesOutput.ReadOnly = true;
            this.variablesOutput.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.variablesOutput.Size = new System.Drawing.Size(223, 522);
            this.variablesOutput.TabIndex = 17;
            // 
            // graphNameBox
            // 
            this.graphNameBox.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.graphNameBox.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.graphNameBox.FormattingEnabled = true;
            this.graphNameBox.Location = new System.Drawing.Point(0, 31);
            this.graphNameBox.Name = "graphNameBox";
            this.graphNameBox.Size = new System.Drawing.Size(141, 21);
            this.graphNameBox.TabIndex = 19;
            this.graphNameBox.SelectedIndexChanged += new System.EventHandler(this.graphNameBox_SelectedIndexChanged);
            this.graphNameBox.KeyUp += new System.Windows.Forms.KeyEventHandler(this.graphNameBox_KeyUp);
            // 
            // submitButton
            // 
            this.submitButton.Dock = System.Windows.Forms.DockStyle.Right;
            this.submitButton.Enabled = false;
            this.submitButton.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.submitButton.Location = new System.Drawing.Point(141, 21);
            this.submitButton.Name = "submitButton";
            this.submitButton.Size = new System.Drawing.Size(82, 31);
            this.submitButton.TabIndex = 17;
            this.submitButton.Text = "Submit";
            this.submitButton.UseVisualStyleBackColor = true;
            this.submitButton.Click += new System.EventHandler(this.submitButton_Click_1);
            // 
            // topicInputBox
            // 
            this.topicInputBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.topicInputBox.Font = new System.Drawing.Font("Verdana", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.topicInputBox.FormattingEnabled = true;
            this.topicInputBox.Location = new System.Drawing.Point(0, 0);
            this.topicInputBox.Name = "topicInputBox";
            this.topicInputBox.Size = new System.Drawing.Size(223, 21);
            this.topicInputBox.TabIndex = 16;
            this.topicInputBox.KeyUp += new System.Windows.Forms.KeyEventHandler(this.topicInputBox_KeyUp);
            // 
            // AIMLPadEditor
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(824, 633);
            this.Controls.Add(this.splitContainer4);
            this.Name = "AIMLPadEditor";
            this.Load += new System.EventHandler(this.TextForm_Load);
            this.splitContainer4.Panel1.ResumeLayout(false);
            this.splitContainer4.Panel2.ResumeLayout(false);
            this.splitContainer4.ResumeLayout(false);
            this.splitContainer5.Panel1.ResumeLayout(false);
            this.splitContainer5.Panel1.PerformLayout();
            this.splitContainer5.Panel2.ResumeLayout(false);
            this.splitContainer5.ResumeLayout(false);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.splitContainer6.Panel1.ResumeLayout(false);
            this.splitContainer6.Panel1.PerformLayout();
            this.splitContainer6.Panel2.ResumeLayout(false);
            this.splitContainer6.ResumeLayout(false);
            this.splitContainer7.Panel1.ResumeLayout(false);
            this.splitContainer7.Panel1.PerformLayout();
            this.splitContainer7.Panel2.ResumeLayout(false);
            this.splitContainer7.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel1.PerformLayout();
            this.splitContainer2.Panel2.ResumeLayout(false);
            this.splitContainer2.ResumeLayout(false);
            this.splitContainer3.Panel1.ResumeLayout(false);
            this.splitContainer3.Panel1.PerformLayout();
            this.splitContainer3.Panel2.ResumeLayout(false);
            this.splitContainer3.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private SplitContainer splitContainer4;
        private SplitContainer splitContainer1;
        private SplitContainer splitContainer2;
        private TextBox consoleText;
        private ComboBox consoleInputText;
        private ComboBox thatInputBox;
        private SplitContainer splitContainer3;
        private TextBox variablesOutput;
        private ComboBox graphNameBox;
        private Button submitButton;
        private ComboBox topicInputBox;
        private SplitContainer splitContainer5;
        private MenuStrip menuStrip1;
        private ToolStripMenuItem clientToolStripMenuItem;
        private ToolStripMenuItem loginToolStripMenuItem;
        private ToolStripMenuItem logoutToolStripMenuItem;
        private ToolStripMenuItem simbotStopToolStripMenuItem;
        private ToolStripMenuItem simbotThinkToolStripMenuItem;
        private ToolStripMenuItem simbotOffToolStripMenuItem;
        private ToolStripMenuItem toolsToolStripMenuItem;
        private SplitContainer splitContainer6;
        private SplitContainer splitContainer7;
        private Label label1;
        private ComboBox robotNameBox;
        private ComboBox userNameBox;
        private Label label2;
        private ComboBox dictionaryNameBox;

    }
}
