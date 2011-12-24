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
        private List<UUID> ClothingFolderFolders;
        int current_subfolder =-1;
        private UUID ClothingFolder = UUID.Zero;
        private UUID Owner = UUID.Zero;
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
            client.Network.SimConnected += FashionBotModuleMain_OnSimConnected;
            client.Inventory.ItemReceived += FashionBotModuleMain_OnItemRecieved;
            client.Inventory.FolderUpdated += FashionBotModuleMain_OnFolderUpdated;
            client.Inventory.InventoryObjectOffered += FashionBotModuleMain_OnObjectOffered;
        }

        private void FashionBotModuleMain_OnObjectOffered(object sender, InventoryObjectOfferedEventArgs e)
        {
            var offerdetails = e.Offer;
            if (offerdetails.FromAgentID == client.MasterKey ||
                offerdetails.FromAgentName.ToLower() == client.MasterName.ToLower())
            {
                UUID objectid = e.ObjectID;
                AssetType type = e.AssetType;
                ReceiveAndUse(objectid, type);
                e.Accept = true;
            }
            else
            {
                e.Accept = false;
            }
        }

        private void ReceiveAndUse(UUID objectid, AssetType type)
        {
            if (type==AssetType.Clothing)
            {
                Received.Add(objectid);
            }
        }

        private void FashionBotModuleMain_OnFolderUpdated(object sender, FolderUpdatedEventArgs e)
        {
            EnumerateClothing();
        }

        private void FashionBotModuleMain_OnItemRecieved(object sender, ItemReceivedEventArgs e)
        {
            var item = e.Item;
            if (Received.Contains(item.AssetUUID))
            {
                List<InventoryItem> newList = new List<InventoryItem> { item };
                client.Appearance.AddToOutfit(newList);
            }
            EnumerateClothing();
        }

        private void FashionBotModuleMain_OnSimConnected(object sender, SimConnectedEventArgs e)
        {
            EnumerateClothing();
            if (ClothingFolderFolders.Count>0)
            {
                StartRotatingClothing();
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
            if (ClothingFolderFolders.Count <= current_subfolder) return;
            // Add new stuff
            UUID folder = ClothingFolderFolders[current_subfolder];
            List<InventoryItem> items = client.GetFolderItems(folder);
            client.Appearance.ReplaceOutfit(items);
            Thread.Sleep(60000);
        }

        private void EnumerateClothing()
        {
            Owner = client.Self.AgentID;
            if (EnumeratingClothing) return;
            EnumeratingClothing = true;
            ClothingFolder = client.Inventory.FindFolderForType(AssetType.Clothing);
            ClothingFolderFolders = new List<UUID>();
            List <InventoryBase> ib = client.Inventory.FolderContents(ClothingFolder, Owner, true, false, InventorySortOrder.ByName, 10000);
            foreach (var list in ib)
            {
              if (list is InventoryFolder)
              {
                  ClothingFolderFolders.Add(list.UUID);
              }   
            }
            EnumeratingClothing = false;
        }

        public override void Dispose()
        {
            //throw new NotImplementedException();
        }
    }
}
