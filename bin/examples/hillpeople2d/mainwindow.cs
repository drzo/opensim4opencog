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

        private static Cursor grabber = new Cursor("..\\..\\..\\grabber.cur");
        private System.Drawing.Bitmap world_map;
        private System.Drawing.Bitmap nav_map;

        private TileManager tm;

        public mainwindow()
        {
            world_map = new Bitmap("..\\..\\..\\rsrc\\worldmap.bmp");
            nav_map = new Bitmap("..\\..\\..\\rsrc\\navmap.png");
            tm = new TileManager(world_map);

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

            tm.draw(g);
        }

        private int grabber_x;
        private int grabber_y;
        private bool is_grabbing = false;

        private void playfieldcontainer_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Middle)
            {
                is_grabbing = true;
                grabber_x = e.X;
                grabber_y = e.Y;
                this.Cursor = grabber;
            }
        }

        private void playfieldcontainer_MouseUp(object sender, MouseEventArgs e)
        {
            if (is_grabbing)
            {
                tm.offsetOrigin(e.X - grabber_x, e.Y - grabber_y);
                this.playfieldcontainer.Invalidate();
                this.navpanel.Invalidate();
             //   this.Invalidate();
                is_grabbing = false;
                this.Cursor = Cursors.Arrow;
                return;
            }
        }

        private void navpanel_Paint(object sender, PaintEventArgs e)
        {
            Graphics g = e.Graphics;

            g.DrawImage(this.nav_map, 0, 0, 128, 74);

            Point p = tm.getOrigin();
            p.X /= -256;
            p.Y /= -256;
            p.Y += 37;
            Brush b = new SolidBrush(Color.FromArgb(128, 0, 0, 0));
            g.FillRectangle(b, p.X, p.Y, 13, 8);
        }

        private void navpanel_MouseUp(object sender, MouseEventArgs e)
        {
            tm.setOrigin(e.X * -256, (e.Y - 37) * -256);
            this.playfieldcontainer.Invalidate();
            this.navpanel.Invalidate();
        }
    }
}
