using System;
using System.Collections.Generic;
using System.Text;

namespace RaptorDB
{
    public class Global
    {
        public static int BitmapOffsetSwitchOverCount = 10;

        public static ushort PageItemCount = 32713; //10000; (primes: 769,1543,3079,6151,
        // 12289,24593,30013,31973,32713, 54251, 65213, 68111,
                                                    // 71993, 72073,89041, 91193, 93563)

        public static int SaveTimerSeconds = 3000;

        public static byte DefaultStringKeySize = 255; //60;

        public static bool FlushStorageFileImmetiatley = true; //false;

        public static bool FreeBitmapMemoryOnSave = false;
    }
}
