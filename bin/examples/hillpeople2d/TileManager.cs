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
                    board[x, y] = new TileArea("grass");
                }
            }
        }

        private static Dictionary<Color, String> mapkey =
            new Dictionary<Color, string>()
            {
                {Color.Red, "fire"},
                {Color.Yellow, "seating"},  //TODO
                {Color.Green, "grass"},
                {Color.Cyan, "hut"},
                {Color.Blue, "river"}, //TODO
                {Color.White, "mountain"},
                {Color.FromArgb(0,166,81), "forest"},
                {Color.FromArgb(46,49,146), "creek"} //TODO

            };

        private String color_to_tile_type(Color c)
        {
            String winner = "grass";
            float dist = 1000.0f;

            foreach (KeyValuePair<Color, string> kvp in mapkey)
            {
                float adist = (kvp.Key.R - c.R) * (kvp.Key.R - c.R) +
                              (kvp.Key.G - c.G) * (kvp.Key.G - c.G) +
                              (kvp.Key.B - c.B) * (kvp.Key.B - c.B);
                if (adist < dist)
                {
                    winner = kvp.Value;
                    dist = adist;
                    if (adist == 0) return winner; // shortcircuit for efficiency
                }
            }

            return winner;
        }

        public TileManager(Bitmap world_map)
        {
            for (int y = 0; y < SIZE_Y; y++)
            {
                for (int x = 0; x < SIZE_X; x++)
                {
                    Color c = world_map.GetPixel(x, y);
                    String tile_name = color_to_tile_type(c);

                    board[x, y] = new TileArea(tile_name);
                }
            }
        }

        public const int U_CELL_OFFSET_X = 64;
        public const int U_CELL_OFFSET_Y = -37;
        public const int V_CELL_OFFSET_X = 64;
        public const int V_CELL_OFFSET_Y = 37;

        private int origin_x = 0;
        private int origin_y = 0;

        public void draw(Graphics g)
        {
            for (int v = 0; v < SIZE_Y; v++)
            {
                for (int u = SIZE_X - 1; u >= 0; u--)
                {
                    int x = origin_x + u * U_CELL_OFFSET_X + v * V_CELL_OFFSET_X;
                    int y = origin_y + u * U_CELL_OFFSET_Y + v * V_CELL_OFFSET_Y;

                    board[u, v].draw(g, x, y);
            //        if( x >=0 && x <= 512 && y >= 0 && y <= 512)
            //            g.DrawString("(" + u + ","+ v + ")", SystemFonts.DefaultFont, Brushes.Black, x + 120, y - 30);
                }
            }             
        }

        internal void offsetOrigin(int x, int y)
        {
            origin_x += x;
            origin_y += y;
        }

        // return origin - might not be on the interior of the data set
        internal Point getOrigin()
        {
            return new Point(
                origin_x,
                origin_y
                );
        }

        internal void setOrigin(int x, int y)
        {
            origin_x = x;
            origin_y = y;
        }
    }
}
