using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using cogbot.TheOpenSims.Mesher;

namespace cogbot.TheOpenSims.Navigation.Debug
{
    internal partial class WaypointProperties : Form
    {
 
        CollisionIndex Current;
        CollisionPlane LastPlane;
        List<Button> CurrentButtons = new List<Button>();

        internal void SetWaypoint(CollisionIndex WP,CollisionPlane p)
        {
            LastPlane = p;
            if (this.button1 == null)
            {
                InitializeComponent();
            }
            Current = WP;
            this.Text = WP.ToString();// +" matrix=" + WP.GetMatrix((float)WP.Position.Z);
            this.button1.Text = WP.ExtraInfoString(LastPlane);
            int i = 0;

            foreach (Button B in CurrentButtons)
            {
                Controls.Remove(B);
            }

            CurrentButtons.Clear();

            foreach (SimMesh O in WP.GetOccupied(LastPlane))
            {
                i++;
                Button B = new Button();
                B.Location = new System.Drawing.Point(24, 28+i*20);
                B.Name = "o"+i;
                B.Size = new System.Drawing.Size(715, 23);
                B.TabIndex = i;
                B.Text = O.OuterBox + " " + O.ToString();
                B.TextAlign = ContentAlignment.MiddleLeft;
                B.UseVisualStyleBackColor = true;
                B.Click += new System.EventHandler(this.object_click);
                this.Controls.Add(B);
                CurrentButtons.Add(B);
            }

        }

        private void WaypointProperties_Load(object sender, EventArgs e)
        {

        }

        private void object_click(object sender, EventArgs e)
        {
            IList<SimMesh> occs = Current.GetOccupied(LastPlane);
            try
            {
                if (sender is Button)
                {
                    Button B = (Button)sender;
                    string name = B.Name.Substring(1);
                    int i = int.Parse(name) - 1;
                    SimMesh O = occs[i];
                    O.RemeshObject();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("" + ex);
            }
        }

        private void button1_Click_1(object sender, EventArgs e)
        {
            Current.RemeshObjects();
            SetWaypoint(Current,LastPlane);
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }
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
            if (button1 != null) return;
            this.button1 = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(12, 18);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(800, 20);
            this.button1.TabIndex = 0;
            this.button1.Text = "Taint";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click_1);
            // 
            // WaypointProperties
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(862, 281);
            this.Controls.Add(this.button1);
            this.Name = "WaypointProperties";
            this.Text = "WaypointProperties";
            this.Load += new System.EventHandler(this.WaypointProperties_Load);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button button1;

        private void textBox1_TextChanged_1(object sender, EventArgs e)
        {

        }


    }
}