using System;
using System.Collections.Generic;
using System.Text;

namespace RaptorDB
{
    public class Global
    {
        public static int BitmapOffsetSwitchOverCount = 10;

        public static ushort PageItemCount = 1543; //10000; (primes: 769,1543,3079,6151,12289,24593)

        public static int SaveTimerSeconds = 3000;

        public static byte DefaultStringKeySize = 64; //60;

        public static bool FlushStorageFileImmetiatley = true; //false;

        public static bool FreeBitmapMemoryOnSave = false;
    }
}
