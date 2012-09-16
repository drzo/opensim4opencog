using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

/*
 * 
 *   handles all the tile drawing 
 *   
 *   tiles are the lozenge shaped pieces of ground.
 *   this class knows which ones are visible and draws them
 */
namespace hillpeople2d
{
    // flyweight pattern
    // base tile images
    public class Tile
    {
        private static Dictionary<String, Tile> bm_set = new Dictionary<string, Tile>();

        private const int Y_OFFSET = -92;  // dist to left point of diamond

        private Image bm;
        private String name;

         private Tile(String name) {
               this.name = name;
               bm = new Bitmap("..\\..\\img\\" + name + ".png");
         }

         public static Tile getTile(String name){
             if (bm_set.ContainsKey(name))
             {
                 return bm_set[name];
             }
             else
             {
                 Tile t = new Tile(name);
                 bm_set[name] = t;
                 return t;
             }
         }

        public void draw(Graphics g, int x , int y) {
            g.DrawImageUnscaled(bm, x, y + Y_OFFSET);
        }
    }

    // mutable object that represents an area. might have additional resources 
    // e.g. an object
    internal class TileArea {
        Tile tile;

        internal TileArea(String name)
        {
            tile = Tile.getTile(name);

            // TODO this needs to know where it is??
            // minimize storage, lots of these.
        }

        // x,y is left point of diamond
        public void draw(Graphics g, int x, int y)
        {
            tile.draw(g, x, y);
        }
    }

    class TileManager
    {
        private const int SIZE_X = 256;
        private const int SIZE_Y = 256;

        private TileArea[,] board = new TileArea[SIZE_X, SIZE_Y];

        public TileManager()
        {
            for(int y = 0; y < SIZE_Y; y++)
            {
                for (int x = 0; x < SIZE_X; x++)
                {
                    board[x, y] = new TileArea("field");
                }
            }
        }

        public const int CELL_OFFSET_X = 64;
        public const int CELL_OFFSET_Y = -37;

        public void draw(Graphics g)
        {
            int sx = 0;
            int sy = 0;
            int rx = 0;
            int ry = 0;

            for (int y = 0; y < SIZE_Y; y++)
            {
                sx = rx;
                sy = ry;
                for (int x = 0; x < SIZE_X; x++)
                {
                    board[x, y].draw(g, sx, sy);
                    sx += CELL_OFFSET_X;
                    sy += CELL_OFFSET_Y;
                }
                rx += CELL_OFFSET_X;
                ry -= CELL_OFFSET_Y; // note cleverness warning
            }             
        }
    }
}
