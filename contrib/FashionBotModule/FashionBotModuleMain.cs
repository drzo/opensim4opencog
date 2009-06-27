using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot;
using cogbot.Listeners;
using OpenMetaverse;

namespace FashionBotModule
{
    public class FashionBotModuleMain : WorldObjectsModule
    {
        private bool EnumeratingClothing;
        private List<InventoryBase> ClothingFolderItems;
        private List<InventoryBase> ClothingFolderFolders;
        int current_subfolder =-1;
        private UUID ClothingFolder;
        private UUID Owner;
        private List<UUID> Received = new List<UUID>();

        public FashionBotModuleMain(BotClient _parent) : base(_parent)
        {
        }

        public override string GetModuleName()
        {
            return this.GetType().Namespace;
        }

        public override void StartupListener()
        {
            client.Network.OnSimConnected += FashionBotModuleMain_OnSimConnected;
            client.Inventory.OnItemReceived += FashionBotModuleMain_OnItemRecieved;
            client.Inventory.OnFolderUpdated += FashionBotModuleMain_OnFolderUpdated;
            client.Inventory.OnObjectOffered += FashionBotModuleMain_OnObjectOffered;
        }

        private bool FashionBotModuleMain_OnObjectOffered(InstantMessage offerdetails, AssetType type, UUID objectid, bool fromtask)
        {
            
            if (offerdetails.FromAgentID==client.MasterKey || offerdetails.FromAgentName.ToLower()==client.MasterName.ToLower())
            {
                ReceiveAndUse(objectid, type);
                return true;
            }
            return false;
        }

        private void ReceiveAndUse(UUID objectid, AssetType type)
        {
            if (type==AssetType.Clothing)
            {
                Received.Add(objectid);
            }
        }

        private void FashionBotModuleMain_OnFolderUpdated(UUID folderid)
        {
            EnumerateClothing();
        }

        private void FashionBotModuleMain_OnItemRecieved(InventoryItem item)
        {
            if (Received.Contains(item.AssetUUID))
            {
                List<InventoryBase> newList = new List<InventoryBase> {item};
                client.Appearance.AddToOutfit(newList, true);
            }
            EnumerateClothing();
        }

        private void FashionBotModuleMain_OnSimConnected(Simulator simulator)
        {
            EnumerateClothing();
            WearDefaults();
            if (ClothingFolderFolders.Count>0)
            {
                StartRotatingClothing();
            }
        }

        private void WearDefaults()
        {
            if (current_subfolder == -1)
            {
                client.Appearance.WearOutfit(ClothingFolder, true);
            }
        }

        private void StartRotatingClothing()
        {
            while (true)
            {
                if (current_subfolder + 1 > ClothingFolderFolders.Count)
                {
                    current_subfolder = -1;
                }
                current_subfolder++;
                WearOutfitTimmed();
                
            }
        }

        private void WearOutfitTimmed()
        {
            // replace with our default first
            client.Appearance.WearOutfit(ClothingFolder, true);
            if (ClothingFolderFolders.Count <= current_subfolder) return;
            // Add new stuff
            AddToOutfit(ClothingFolderFolders[current_subfolder]);
            Thread.Sleep(60000);
        }

        private void AddToOutfit(InventoryBase folder)
        {
            List<InventoryBase> items = client.Inventory.FolderContents(folder.UUID, Owner, false, true, InventorySortOrder.ByDate, 10000);
            client.Appearance.AddToOutfit(items, true);
        }

        private void EnumerateClothing()
        {
            Owner = client.Self.AgentID;
            if (EnumeratingClothing) return;
            EnumeratingClothing = true;
            ClothingFolder = client.Inventory.FindFolderForType(AssetType.Clothing);
            ClothingFolderFolders = client.Inventory.FolderContents(ClothingFolder, Owner, true, false, InventorySortOrder.ByName, 10000);
            ClothingFolderItems = client.Inventory.FolderContents(ClothingFolder, Owner, false, true, InventorySortOrder.ByName, 10000);
            EnumeratingClothing = false;
        }

        public override void ShutdownListener()
        {
            //throw new NotImplementedException();
        }
    }
}
