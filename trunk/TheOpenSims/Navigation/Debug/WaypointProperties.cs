using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace cogbot.TheOpenSims.Navigation.Debug
{
    public partial class WaypointProperties : Form
    {
        public WaypointProperties()
        {
            InitializeComponent();
        }

        SimWaypoint Current;
        List<Button> CurrentButtons = new List<Button>();

        internal void SetWaypoint(SimWaypoint WP)
        {
            Current = WP;
            this.Text = WP.ToString() + " matrix=" + WP.GetMatrix();
            this.textBox1.Text = " GLevel=" + WP.GetGroundLevel() + " OMin/MaxZ=" + WP.OMinZ + "/" + WP.OMaxZ + " ZLevel=" + WP.GetZLevel();

            int i = 0;

            foreach (Button B in CurrentButtons)
            {
                Controls.Remove(B);
            }

            CurrentButtons.Clear();

            foreach (SimObject O in WP.OccupiedListObject)
            {
                i++;
                Button B = new Button();
                B.Location = new System.Drawing.Point(24, 28+i*20);
                B.Name = "o"+i;
                B.Size = new System.Drawing.Size(715, 23);
                B.TabIndex = i;
                B.Text = O.GetMinMaxZ(WP.Point) + " " + O.ToString();
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
            try
            {
                if (sender is Button)
                {
                    Button B = (Button)sender;
                    string name = B.Name.Substring(1);
                    int i = int.Parse(name) - 1;
                    SimObject O = Current.OccupiedListObject[i];
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
            Current.RemeshWayppointObjects();
            SetWaypoint(Current);
        }

    }
}