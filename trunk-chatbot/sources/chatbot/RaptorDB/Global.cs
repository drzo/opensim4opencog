using System;
using System.Collections.Generic;
using System.Text;

namespace RaptorDB
{
    public class Global
    {
        /// <summary>
        /// Store bitmap as int offsets then switch over to bitarray
        /// </summary>
        public static int BitmapOffsetSwitchOverCount = 10;
        /// <summary>
        /// True = Save to other views in process , False = background save to other views
        /// </summary>
        public static bool BackgroundSaveToOtherViews = true;

        public static byte DefaultStringKeySize = 255; //60;

        public static bool FreeBitmapMemoryOnSave = true;

        public static ushort PageItemCount = 10193; //10000; (primes: 769,1543,3079,4093,6151,10193
        // 12289,16193,24593,30013,31973,32713, 54251, 65213, 68111,
                                                    // 71993, 72073,89041, 91193, 93563)

        public static int SaveTimerSeconds = 3000;


        //public static ushort PageItemCount = 10000;
        /// <summary>
        /// KeyStore save to disk timer
        /// </summary>
        public static int SaveIndexToDiskTimerSeconds = 60;
        /// <summary>
        /// Flush the StorageFile stream immediatley
        /// </summary>
        public static bool FlushStorageFileImmetiatley = true;
        /// <summary>
        /// Save doc as binary json
        /// </summary>
        public static bool SaveAsBinaryJSON = true;
        /// <summary>
        /// Remove completed tasks timer
        /// </summary>
        public static int TaskCleanupTimerSeconds = 3;
        /// <summary>
        /// Save to other views timer seconds if enabled 
        /// </summary>
        public static int BackgroundSaveViewTimer = 1;
        /// <summary>
        /// How many items to process in a background view save event
        /// </summary>
        public static int BackgroundViewSaveBatchSize = 1000000;
        /// <summary>
        /// Check the restore folder for new backup files to restore
        /// </summary>
        public static int RestoreTimerSeconds = 10;
        /// <summary>
        /// Timer for full text indexing of original documents
        /// </summary>
        public static int FullTextTimerSeconds = 60;
    }
}
