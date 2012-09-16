using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Drawing.Drawing2D;

namespace hillpeople2d
{
    public partial class mainwindow : Form
    {
        private const int BOTAREA_WIDTH = 124;

        private System.Drawing.Image test;

        private TileManager tm;

        public mainwindow()
        {
            tm = new TileManager();

            test = new Bitmap("..\\..\\img\\field_ripe.png");
            InitializeComponent();
        }

        private void botarea_Resize(object sender, EventArgs e)
        {
            Console.Out.WriteLine("botarea resize");
           this.botarea.Size = new Size(BOTAREA_WIDTH, this.Height);
        }
       
        private void mainwindow_Resize(object sender, EventArgs e)
        {
            Console.Out.WriteLine("main window resize");
           this.botarea.Size = new Size(BOTAREA_WIDTH, this.Height);
        }

        private void playfieldcontainer_Paint(object sender, PaintEventArgs e)
        {
            Graphics g = e.Graphics;

            g.DrawImageUnscaled(test, 200, 200);
            g.DrawImageUnscaled(test, 230, 230);
            g.DrawLine(Pens.Black, 0.0f, 0.0f, 100.0f, 200.0f);
            tm.draw(g);
        }
    }
}
