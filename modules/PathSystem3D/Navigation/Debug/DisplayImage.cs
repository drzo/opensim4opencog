using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace PathSystem3D.Navigation.Debug
{
    public partial class DisplayImage : Form
    {
        public DisplayImage(String name,Image show)
        {
            InitializeComponent();
            pictureBox1.Image = show;
            this.Text = name;
            Visible = true;
        }
    }
}