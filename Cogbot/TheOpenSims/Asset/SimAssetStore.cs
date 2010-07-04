using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Xml;
using cogbot.Listeners;
using cogbot.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    public class SimAssetStore
    {

        //internal static readonly Dictionary<UUID, object> uuidAsset = new Dictionary<UUID, object>();

        static internal Dictionary<UUID, object> uuidAsset
        {
            get { return WorldObjects.uuidTypeObject; }
        }

        public static SimAssetStore TheStore;

        internal static readonly Dictionary<string, SimAsset> nameAsset = new Dictionary<string, SimAsset>();

        internal readonly BotClient Client;

        static private readonly TaskQueueHandler taskQueue = new TaskQueueHandler("SimAssetStore", 1, false);

        public static void TaskQueueStart()
        {
            lock (taskQueue)
            {
                if (!taskQueue.IsRunning) taskQueue.Start();
            }
        }
        private InventoryManager Manager;
        private OpenMetaverse.Inventory Inventory;

        HashSet<UUID> BusyUpdating = new HashSet<UUID>();
        public SimAssetStore(BotClient GC)
        {
            TheStore = this;
            Client = GC;           
            Manager = Client.Inventory;
            Manager.ItemReceived += Inventory_OnItemReceived;
            Manager.TaskItemReceived += Inventory_OnTaskItemReceived;
            Manager.FolderUpdated += Inventory_OnFolderUpdated;
            Inventory = Manager.Store;
            Client.Network.SimConnected += Ensure_Downloaded;
            FillAssetNames();
        }


        private void Store_OnInventoryObjectRemoved(object sender, InventoryObjectRemovedEventArgs e)
        {
           // throw new NotImplementedException();
        }

        private void Store_OnInventoryObjectUpdated(object sender, InventoryObjectUpdatedEventArgs e)
        {
            Enqueue(() =>LoadItemOrFolder(e.NewObject));
        }

        private void Store_OnInventoryObjectAdded(object sender, InventoryObjectAddedEventArgs e)
        {
            Enqueue(() =>LoadItemOrFolder(e.Obj));
        }

        private void Inventory_OnFolderUpdated(object sender, FolderUpdatedEventArgs e)
        {
            var folderid = e.FolderID;
            lock (BusyUpdating) if (BusyUpdating.Contains(folderid)) return;
            LoadFolderId(folderid);
         //   lock (BusyUpdating) if (BusyUpdating.Remove(folderid)) return;
        }

        private void Inventory_OnTaskItemReceived(object sender, TaskItemReceivedEventArgs e)
        {
            if (e.Type == InventoryType.Object) return;
            var folderid = e.FolderID;
            //   SimAsset A = SetAssetName(assetid, null, type);
            LoadFolderId(folderid);
            lock (BusyUpdating) if (BusyUpdating.Remove(folderid)) return;
        }

        private void LoadFolderId(UUID folderid)
        {
            if (!WorldObjects.GleanAssetsFromFolders) return;
            if (folderid == UUID.Zero) return;
            lock (BusyUpdating) if (BusyUpdating.Contains(folderid)) return;
            lock (BusyUpdating) BusyUpdating.Add(folderid);
            Enqueue(() =>
                                  {
                                      List<InventoryBase> contents = Client.Inventory.FolderContents(folderid,
                                                                                                     Client.Self.AgentID,
                                                                                                     true, true,
                                                                                                     InventorySortOrder.
                                                                                                         ByName, 10000);
                                      if (contents != null) contents.ForEach(LoadItemOrFolder);
                                  //    lock (BusyUpdating) BusyUpdating.Remove(folderid);
                                  });

        }

        private void Inventory_OnItemReceived(object sender, ItemReceivedEventArgs e)
        {
            Enqueue(() => LoadItemOrFolder(e.Item));
        }

        private bool downloadedAssetFolders = false;
        static bool downloadedAssetFoldersComplete = false;
        private void Ensure_Downloaded(object sender, SimConnectedEventArgs e)
        {
            if (downloadedAssetFolders) return;
            downloadedAssetFolders = true;
            Inventory = Manager.Store;
            Inventory.InventoryObjectAdded += Store_OnInventoryObjectAdded;
            Inventory.InventoryObjectUpdated += Store_OnInventoryObjectUpdated;
            Inventory.InventoryObjectRemoved += Store_OnInventoryObjectRemoved;
            Enqueue((DownloadAssetFolders));

        }

         void DownloadAssetFolders()
        {           
            Client.AnimationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
            LoadFolderId(Client.Inventory.Store.LibraryFolder.UUID);
            LoadFolderId(Client.Inventory.Store.RootFolder.UUID);
            downloadedAssetFoldersComplete = true;
        }

        private void LoadItemOrFolder(InventoryBase IB)
        {
            if (IB is InventoryItem)
            {
                InventoryItem II = (InventoryItem)IB;
                if (II.AssetType == AssetType.Object) return;
                SimAsset A = SetAssetName(II.AssetUUID, II.Name, II.AssetType);
                A.Item = II;
                return;
            }
            LoadFolderId(IB.UUID);
            lock (BusyUpdating) if (BusyUpdating.Remove(IB.UUID)) return;
            //lock (BusyUpdating) BusyUpdating.Add(IB.UUID);
            //List<InventoryBase> contents = Client.Inventory.FolderContents(IB.UUID, Client.Self.AgentID,
            //    true, true, InventorySortOrder.ByName, 30000);
            //if (contents != null) foreach (InventoryBase IBO in contents)
            //{
            //    LoadItemOrFolder(IBO);
            //}
            //lock (BusyUpdating) BusyUpdating.Remove(IB.UUID);
        }


        internal void OnAssetDownloaded(UUID uUID, Asset asset)
        {
            SimAsset A = FindOrCreateAsset(uUID, asset.AssetType);
            if (A.HasData())
            {
                
            }
            A.ServerAsset = asset;
            InternAsset(A);
            //A.TypeData = asset.AssetData;




            if (false)
            {
                AutoResetEvent UploadCompleteEvent = new AutoResetEvent(false);
                if (Client.AnimationFolder == UUID.Zero)
                    Client.AnimationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);

                DateTime start = new DateTime();
                Client.Inventory.RequestCreateItemFromAsset(asset.AssetData, A.Name, "Anim captured " + uUID,
                                                            AssetType.Animation,
                                                            InventoryType.Animation, Client.AnimationFolder,
                                                            delegate(bool success, string status, UUID itemID,
                                                                     UUID assetID)
                                                            {
                                                                WriteLine(
                                                                    String.Format(
                                                                        "RequestCreateItemFromAsset() returned: Success={0}, Status={1}, ItemID={2}, AssetID={3}",
                                                                        success, status, itemID, assetID));
                                                                WriteLine(String.Format("Upload took {0}",
                                                                                                DateTime.Now.
                                                                                                    Subtract(start)));
                                                                UploadCompleteEvent.Set();
                                                            });

                UploadCompleteEvent.WaitOne();

                //A.Name
                //SetAnimationName(asset.AssetID, s);
            }
            //              Debug(s);
            //                        RegisterUUID(asset.AssetID, s);

        }

        static void InternAsset(SimAsset asset)
        {
            WorldObjects.GridMaster.SendNewRegionEvent(SimEventType.DATA_UPDATE,"OnAssetInfo",asset);
        }


        static void AddSound(string p, string p_2, string p_3, string p_4, string p_5)
        {
            AddSound(p, p_2);
        }

        static void AddSound(string p, string p_2, string p_3)
        {
            AddSound(p, p_3);
        }


        static void AddSound(string fName, string id)
        {
            fName = fName.ToLower();
            UUID uid = UUID.Parse(id);
            SimAsset sound = FindOrCreateAsset(uid, AssetType.Sound);
            sound.Name = fName;
            WorldObjects.RegisterUUID(uid, sound);
            if (Directory.Exists("sound_files/"))
            {
                byte[] bytes;
                string usedName;
                if (BytesFromFile(fName, out bytes, out usedName))
                {
                    sound.AssetData = bytes;
                    sound.Name = usedName;
                }
                else
                {
                    WriteLine("Sound w/o BVH " + fName + " " + uid);
                }
            }
            InternAsset(sound);
        }

        static internal void FillAssetNames()
        {
            if (FilledInAssets) return;
            lock (uuidAsset)
            {
                if (FilledInAssets) return; 
                FilledInAssets = true;
                AddTexture("alpha_gradient", "e97cf410-8e61-7005-ec06-629eba4cd1fb","Used for generating the texture for the ground dynamically. Also used for creating the *invisiprim* that hides avatars, prims with alpha values less than 1.0 and particle effects");
                AddTexture("alpha_gradient_2d", "38b86f85-2575-52a9-a531-23108d8da837",
                           "Used for generating the texture for the ground dynamically. Also used for creating the *invisiprim* that hides avatars, prims with alpha values less than 1.0 and particle effects ");
                AddTexture("collision texture", "35f217a3-f618-49cf-bbca-c86d486551a9",
                           "Sprite used to generate collision visual effect.");
                AddTexture("grass_texture_1", "79504bf5-c3ec-0763-6563-d843de66d0a1");
                AddTexture("grass_texture_2", "6c4727b8-ac79-ba44-3b81-f9aa887b47eb");
                AddTexture("grass_texture_3", "99bd60a2-3250-efc9-2e39-2fbcadefbecc");
                AddTexture("grass_texture_4", "7a2b3a4a-53c2-53ac-5716-aac7d743c020");
                AddTexture("noentrylines", "5d3e196b-fd4d-ada7-e4c1-99f8e9f1cfbf",
                           "Red *No entry of property* lines.");
                AddTexture("noentrypasslines", "ac8f8627-6a30-8da8-d4bd-958668eea7a0",
                           "Green *No entry of property, except with pass* lines. ");
                AddTexture("propertyline", "e3548c46-8d5e-03da-fcab-4fc36ad818bb",
                           "Semi-transparent texture overlayed on the ground along the borders of a property when View>Show Property Lines is active. ");
                AddTexture("terrain_dirt", "b8d3965a-ad78-bf43-699b-bff8eca6c975", "Ground texture.");
                AddTexture("terrain_dirt_detail", "0bc58228-74a0-7e83-89bc-5c23464bcec5", "Hi-res ground texture.");
                AddTexture("terrain_grass", "abb783e6-3e93-26c0-248a-247666855da3", "Ground texture.");
                AddTexture("terrain_grass_detail", "63338ede-0037-c4fd-855b-015d77112fc8", "Hi-res ground texture.");
                AddTexture("terrain_mountain", "179cdabd-398a-9b6b-1391-4dc333ba321f", "Ground texture.");
                AddTexture("terrain_mountain_detail", "303cd381-8560-7579-23f1-f0a880799740", "Hi-res ground texture.");
                AddTexture("terrain_rock", "beb169c7-11ea-fff2-efe5-0f24dc881df2", "Ground texture.");
                AddTexture("terrain_rock_detail", "53a2f406-4895-1d13-d541-d2e3b86bc19c", "Hi-res ground texture.");
                AddTexture("tree_beach_grass_1", "18fb888b-e8f1-dce7-7da7-321d651ea6b0");
                AddTexture("tree_cypress_1", "fb2ae204-3fd1-df33-594f-c9f882830e66");
                AddTexture("tree_cypress_2", "30047cec-269d-408e-0c30-b2603b887268");
                AddTexture("tree_dogwood", "64367bd1-697e-b3e6-0b65-3f862a577366");
                AddTexture("tree_eelgrass", "96b4de31-f4fa-337d-ec78-451e3609769e");
                AddTexture("tree_eucalyptus", "a6162133-724b-54df-a12f-51cd070ad6f3");
                AddTexture("tree_fern", "8872f2b8-31db-42d8-580a-b3e4a91262de");
                AddTexture("tree_kelp_1", "2caf1179-7861-6ff3-4b7d-46e17780bdfa");
                AddTexture("tree_kelp_2", "2a4880b6-b7a3-690a-2049-bfbe38eafb9f");
                AddTexture("tree_oak", "8a515889-eac9-fb55-8eba-d2dc09eb32c8");
                AddTexture("tree_palm_1", "ca4e8c27-473c-eb1c-2f5d-50ee3f07d85c");
                AddTexture("tree_palm_2", "2d784476-d0db-9979-0cff-9408745a7cf3");
                AddTexture("tree_pine_1", "0187babf-6c0d-5891-ebed-4ecab1426683");
                AddTexture("tree_pine_2", "d691a01c-13b7-578d-57c0-5caef0b4e7e1");
                AddTexture("tree_plumeria", "6de37e4e-7029-61f5-54b8-f5e63f983f58");
                AddTexture("tree_sea_sword", "5894e2e7-ab8d-edfa-e61c-18cf16854ba3");
                AddTexture("tree_tropical_1", "5bc11cd6-2f40-071e-a8da-0903394204f9");
                AddTexture("tree_tropical_2", "cdd9a9fc-6d0b-f90d-8416-c72b6019bca8");
                AddTexture("undergrowth_1", "8f458549-173b-23ff-d4ff-bfaa5ea2371b");
                AddTexture("water", "2bfd3884-7e27-69b9-ba3a-3e673f680004",
                           "Semi-transparent blue texture shown on the surface of water.");
                AddTexture("winter_tree_aspen", "7c0cf89b-44b1-1ce2-dd74-07102a98ac2a");
                AddTexture("winter_tree_pine_1", "10d2a01a-0818-84b9-4b96-c2eb63256519");
                AddTexture("winter_tree_pine_2", "67931331-0c02-4876-1255-28770896c6a2");

                AddTexture("silhouette", "da5d4079-7819-6b53-d2a4-dc9929381d7d",
                           "The constant shadow cast by the avatar's feet?");
                AddTexture("avatar_thumb_bkgrnd", "3a7f4f0d-be14-ee78-29e3-fc8b0b2a68d3");
                AddTexture("eyes", "6522e74d-1660-4e7f-b601-6f48c1659a77");
                AddTexture("hair", "7ca39b4c-bd19-4699-aff7-f93fd03d3e7b");
                AddTexture("pixiesmall", "168e6813-096e-07ea-97ae-fd416826f627",
                           "The particle texture used as both the default texture for particles and for avatar editing beams.");


                AddTexture("button_disabled_32x128", "f8124d60-2875-c358-7847-2acb63e5400c");
                AddTexture("button_enabled_32x128", "d8faf8cb-ee6e-b0b5-abd9-bde873ad3461");
                AddTexture("button_enabled_selected_32x128", "1eddba75-b682-110a-104e-6cdcce616a25");
                AddTexture("cam_rotate_in", "70bf2262-3eed-4996-88ac-076907e8921d");
                AddTexture("cam_rotate_out", "88745b46-da05-11d5-8ac0-0003477c4611");
                AddTexture("cam_tracking_in", "fe2fc73b-5a64-4a8e-aacc-46fa81faf96a");
                AddTexture("cam_tracking_out", "95c4ea0e-e3c2-4904-b847-7d7676139ebb");
                AddTexture("cam_zoom_minus_in", "deed3f4b-93e9-4183-a3b0-a5a98a6de1bb");
                AddTexture("cam_zoom_out", "bb02e941-cb3b-4dd3-892a-6841b5de6e45");
                AddTexture("cam_zoom_plus_in", "c7aefd32-ce13-4242-82cc-2631d44ff9d3");
                AddTexture("checkbox_disabled_false", "7d94cb59-32a2-49bf-a516-9e5a2045f9d9");
                AddTexture("checkbox_disabled_true", "c817c642-9abd-4236-9287-ae0513fe7d2b");
                AddTexture("checkbox_enabled_false", "05bb64ee-96fd-4243-b74e-f40a41bc53ba");
                AddTexture("checkbox_enabled_true", "cf4a2ed7-1533-4686-9dde-df9a37ddca55");
                AddTexture("closebox", "47a8c844-cd2a-4b1a-be01-df8b1612fe5d");
                AddTexture("close_in_blue", "e5821134-23c0-4bd0-af06-7fa95b9fb01a");
                AddTexture("close_inactive_blue", "779e4fa3-9b13-f74a-fba9-3886fe9c86ba");
                AddTexture("combobox_arrow", "b31c1335-0e9c-4927-bc90-53277777d9c1");
                AddTexture("crosshairs", "6e1a3980-bf2d-4274-8970-91e60d85fb52", "Texture for the mouselook crosshairs.");
                AddTexture("minimize", "34c9398d-bb78-4643-9633-46a2fa3e9637");
                AddTexture("minimize_inactive", "6e72abba-1378-437f-bf7a-f0c15f3e99a3");
                AddTexture("minimize_pressed", "39801651-26cb-4926-af57-7af9352c273c");
                AddTexture("notify_box_icon", "b2ef2d31-9714-a07b-6ca7-31638166364b");
                AddTexture("notify_next", "07d0ea4c-af0c-aad1-dbbf-c24020ff2b80");
                AddTexture("notify_tip_icon", "74ba3584-58ea-9984-5b76-62d37942ab77");
                AddTexture("radio_active_false", "7a1ba9b8-1047-4d1e-9cfc-bc478c80b63f");
                AddTexture("radio_active_true", "52f09e07-5816-4052-953c-94c6c10479b7");
                AddTexture("radio_inactive_false", "90688481-67ff-4af0-be69-4aa084bcad1e");
                AddTexture("radio_inactive_true", "1975db39-aa29-4251-aea0-409ac09d414d");
                AddTexture("resize_handle_bottom_right_blue", "e3690e25-9690-4f6c-a745-e7dcd885285a");
                AddTexture("restore", "111b39de-8928-4690-b7b2-e17d5c960277");
                AddTexture("restore_inactive", "0eafa471-70af-4882-b8c1-40a310929744");
                AddTexture("restore_pressed", "90a0ed5c-2e7b-4845-9958-a64a1b30f312");
                AddTexture("scrollbutton_down_in_blue", "d2421bab-2eaf-4863-b8f6-5e4c52519247");
                AddTexture("scrollbutton_down_out_blue", "b4ecdecf-5c8d-44e7-b882-17a77e88ed55");
                AddTexture("scrollbutton_left_in_blue", "ea137a32-6718-4d05-9c22-7d570d27b2cd");
                AddTexture("scrollbutton_left_out_blue", "43773e8d-49aa-48e0-80f3-a04715f4677a");
                AddTexture("scrollbutton_right_in_blue", "b749de64-e903-4c3c-ac0b-25fb6fa39cb5");
                AddTexture("scrollbutton_right_out_blue", "3d700d19-e708-465d-87f2-46c8c0ee7938");
                AddTexture("scrollbutton_up_in_blue", "a93abdf3-27b5-4e22-a8fa-c48216cd2e3a");
                AddTexture("scrollbutton_up_out_blue", "dad084d7-9a46-452a-b0ff-4b9f1cefdde9");
                AddTexture("spin_down_in_blue", "a985ac71-052f-48e6-9c33-d931c813ac92");
                AddTexture("spin_down_out_blue", "b6d240dd-5602-426f-b606-bbb49a30726d");
                AddTexture("spin_up_in_blue", "c8450082-96a0-4319-8090-d3ff900b4954");
                AddTexture("spin_up_out_blue", "56576e6e-6710-4e66-89f9-471b59122794");
                AddTexture("square_btn_32x128", "b28df901-6b8d-d31c-7903-4eb9676d4bfc");
                AddTexture("square_btn_selected_32x128", "c48c9e95-191b-96d3-08b2-6e8ada58b651");
                AddTexture("startup_logo", "66864f3c-e095-d9c8-058d-d6575e6ed1b8");
                AddTexture("status_build", "175a6b75-45c9-c2c2-4765-bf37a3909b53", "No-build land status symbol.");
                AddTexture("status_busy", "beb0d821-6725-abdf-032d-1f70cdabde82", "Busy mode symbol.");
                AddTexture("status_fly", "0e058115-5b8f-c3d7-dcaa-9623d92885d1", "No-fly land status symbol.");
                AddTexture("status_health", "4330e8ce-b39b-1eb8-c2ec-a97c0b3947b5", "Health-enabled area symbol.");
                AddTexture("status_money", "5863eb7a-1546-6501-533a-6061f73a36b7", "L$ symbol.");
                AddTexture("status_scripts", "4cc1afcd-04dd-178f-e074-0f9dc730ab45", "No-scripts land status symbol.");
                AddTexture("tab_bottom_blue", "bf0a8779-689b-48c3-bb9a-6af546366ef4");
                AddTexture("tab_bottom_selected_blue", "c001d8fd-a869-4b6f-86a1-fdcb106df9c7");
                AddTexture("tab_left", "1097dcb3-aef9-8152-f471-431d840ea89e");
                AddTexture("tab_left_selected", "bea77041-5835-1661-f298-47e2d32b7a70");
                AddTexture("tab_top_blue", "1ed83f57-41cf-4052-a3b4-2e8bb78d8191");
                AddTexture("tab_top_selected_blue", "16d032e8-817b-4368-8a4e-b7b947ae3889");

                AddTexture("button_anim_pause", "db2d9c2d-0bbd-21e2-e83a-103ea2def7a8");
                AddTexture("button_anim_pause_selected", "ad65d67a-777b-fbfa-693d-4bdcfca2acca");
                AddTexture("button_anim_play", "2a7f6738-5d82-2ff3-d419-30ed09cbb72b");
                AddTexture("button_anim_play_selected", "119c37bb-24af-45fe-ae11-3a6bc3c85138");
                AddTexture("button_anim_stop", "e10c9e36-d9f6-c8b4-de96-557dccce9205");
                AddTexture("button_anim_stop_selected", "b8c0e0aa-2771-439e-c919-d2f5dad69a1c");
                AddTexture("container_animation", "c4e657a1-4c86-0159-2da0-32ff948484e6");
                AddTexture("container_bodypart", "770cb2df-758d-34d5-36c7-e3de06db5b5d");
                AddTexture("container_clothing", "dd90406f-4c8f-a3f9-41df-d562f94f09e0");
                AddTexture("container_gesture", "59cd31c0-2791-3c48-f740-f0a36c68653e",
                           "If a gesture is dragged out of inventory onto the ground, it appears contained in a cube with this texture applied to it.");
                AddTexture("container_landmark", "24c63386-04f7-ce6f-4ff2-dfb215d2e21f",
                           "If a landmark is dragged out of inventory onto the ground, it appears contained in a cube with this texture applied to it.");
                AddTexture("container_many_things", "849d3292-d9fa-7186-5465-dd7b5fc1ec48",
                           "If multiple inventory items (or a folder is) are selected, and dragged on to the ground, they appear in the contents of a cube with this texture applied to it.");
                AddTexture("container_object", "ad887ae1-2bee-f2c9-6786-5599de3c95c4");
                AddTexture("container_script", "b93bd494-c4bd-bcdf-4a59-35a9497d03f3",
                           "If a script is dragged out of inventory onto the ground, it appears contained in a cube with this texture applied to it.");
                AddTexture("container_texture", "5ddea031-cfa3-2776-43e3-c7146c1b4cd6",
                           "If a texture is dragged out of inventory onto the ground, it appears contained in a cube with this texture applied to it.");
                AddTexture("container_texture", "b3f95caf-bd62-bef3-0ded-dea752920629",
                           "If a texture is dragged out of inventory onto the ground, it appears contained in a cube with this texture applied to it.");
                AddTexture("inv_folder_animation", "4d59b3ee-f29d-b912-2bcc-9bb1f8a07ec6",
                           "Icon used for the *Animations* inventory folder.");
                AddTexture("inv_folder_bodypart", "1fe05580-1d2f-0345-b28b-52b6e3a20e5d",
                           "Icon used for the *Bodyparts* inventory folder.");
                AddTexture("inv_folder_callingcard", "a3735971-e2b2-d78a-580d-d265cd8f2484",
                           "Icon used for the *Calling Cards* inventory folder.");
                AddTexture("inv_folder_clothing", "f1427d3d-b2e8-97c4-69ab-1f36d4c0e8f0",
                           "Icon used for the *Clothing* inventory folder.");
                AddTexture("inv_folder_gesture", "4de9129a-9fc1-d759-d739-364293906ba2",
                           "Icon used for the *Gestures* inventory folder.");
                AddTexture("inv_folder_landmark", "9f921155-7c8c-e276-d5ec-03ac9340584d",
                           "Icon used for the *Landmarks* inventory folder.");
                AddTexture("inv_folder_lostandfound", "9a371a04-297d-bacf-0d16-5f49753efe1d",
                           "Icon used for the *Lost and Found* inventory folder.");
                AddTexture("inv_folder_notecard", "a9e75d84-5073-9cb7-10a9-1ca68ef5c7ba",
                           "Icon used for the *Notecards* inventory folder.");
                AddTexture("inv_folder_object", "113e5133-fd0d-ee51-4a59-9d67ca10e8a7",
                           "Icon used for the *Objects* inventory folder.");
                AddTexture("inv_folder_plain_closed", "86f00960-c3e9-9680-145d-3beffd743e9c",
                           "Icon used for closed inventory folders.");
                AddTexture("inv_folder_plain_open", "d15dc243-2d0b-47af-0ce1-ec376464bdc8",
                           "Icon used for open inventory folders.");
                AddTexture("inv_folder_script", "baa5c310-6a6d-cc48-51eb-65196ba31d77",
                           "Icon used for the *Scripts* inventory folder.");
                AddTexture("inv_folder_snapshot", "6efe85e7-800f-1843-296c-a5b7adffe091",
                           "Icon used for the *Snapshots* inventory folder.");
                AddTexture("inv_folder_texture", "e10cb910-1e71-da47-bd12-8c53f7793714",
                           "Icon used for the *Textures* inventory folder.");
                AddTexture("inv_folder_texture", "743f035b-a049-43f4-16c7-7ec8daa2c481",
                           "Icon used for the *Textures* inventory folder.");
                AddTexture("inv_folder_trash", "88ad072e-ea0b-aabd-5ac0-b37862a6eb66",
                           "Icon used for the *Trash* inventory folder.");
                AddTexture("inv_item_animation", "b5cda0d6-d196-ce48-63db-d04323ef8931",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_attach", "5bcae41e-aa5d-02f8-edf1-605ebdd875ab",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_bodypart", "d2a5362d-5c55-57dd-a9e9-5c814d1ddc16",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_callingcard_offline", "d0afe86b-2489-7600-55b7-6abb0a63d9f9",
                           "Icon used to represent an offline calling card.");
                AddTexture("inv_item_callingcard_online", "672cc53e-8dc0-ba91-2a4e-574104cf071c",
                           "Icon used to represent an online calling card.");
                AddTexture("inv_item_clothing", "34dfe476-8e26-0e3a-11cf-76cc4a7126ce",
                           "Icon used to represent a clothing inventory item.");
                AddTexture("inv_item_eyes", "eaa5fd96-5c25-06ef-2280-7ef20203e167",
                           "Icon used to represent eye settings.");
                AddTexture("inv_item_gesture", "5579245d-d5bf-5f13-46b0-8624490de24c",
                           "Icon used to represent a gesture inventory item.");
                AddTexture("inv_item_gloves", "117b11cb-c04e-5081-13da-1a8846070fd0",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_hair", "6bca3bf4-ed6d-d438-63a0-2a7066d03a0b",
                           "Icon used to represent hair settings.");
                AddTexture("inv_item_jacket", "8df59386-56e0-c811-0443-840da3acb3a5",
                           "Icon used to represent a jacket inventory item.");
                AddTexture("inv_item_landmark", "bf25a2a0-85da-7fa0-0993-e461768d0221",
                           "Icon used to represent an unvisited landmark.");
                AddTexture("inv_item_landmark_visited", "229fac85-5428-4ab7-adeb-eb8389e91092",
                           "Icon used to represent a visited landmark.");
                AddTexture("inv_item_notecard", "23ce8a2c-9ea2-d863-6572-806f0645b0c7",
                           "Icon used to represent a notecard inventory item.");
                AddTexture("inv_item_object", "0f0780a0-89c4-742a-ef28-26405a41cf85",
                           "Icon used to represent a object inventory item.");
                AddTexture("inv_item_pants", "a87a58ca-f857-63b1-0acf-072711ed1bdb",
                           "Icon used to represent a pants inventory item.");
                AddTexture("inv_item_script", "59a3df81-ed76-06c9-7264-6dada535e7a3",
                           "Icon used to represent a script inventory item.");
                AddTexture("inv_item_shape", "4463e433-4db5-79ef-c1b0-4821b03ddb07",
                           "Icon used to represent a shape inventory item.");
                AddTexture("inv_item_shirt", "e2ffb62b-6abc-22d6-952d-764759b4d636",
                           "Icon used to represent a shirt inventory item.");
                AddTexture("inv_item_shoes", "cf384fa5-1edd-c37c-2134-283dd4fe3396",
                           "Icon used to represent a shoes inventory item.");
                AddTexture("inv_item_skirt", "0b43f826-2abc-2944-7d72-10777a51d19b",
                           "Icon used to represent a skirt inventory item.");
                AddTexture("inv_item_snapshot", "3810d584-b092-7caa-57e0-010f192b9659",
                           "Icon used to represent a snapshot inventory item.");
                AddTexture("inv_item_socks", "22137c6d-6ec5-6eee-9a2e-2d7a9e6cbcd4",
                           "Icon used to represent a socks inventory item.");
                AddTexture("inv_item_texture", "eb414d69-c77d-d4e7-66e6-6c2e6f6c1976",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_texture", "19f452d7-4eee-9f46-76cc-5497d17f1dd9",
                           "Icon used to represent a texture inventory item.");
                AddTexture("inv_item_underpants", "2f15dc09-4385-526c-aa5d-d9d516ec7d99",
                           "Icon used to represent a underpants inventory item.");
                AddTexture("inv_item_undershirt", "f72ab629-a3ab-de0c-35c0-5285e27478ce",
                           "Icon used to represent a undershirt inventory item.");

                AddTexture("compass markers", "79156764-de98-4815-9d50-b10a7646bcf4");
                AddTexture("direction_arrow", "586383e8-4d9b-4fba-9196-2b5938e79c2c");
                AddTexture("map_avatar_16", "db0dadd5-026a-88cf-f5c1-523a0a2daa3e");
                AddTexture("map_avatar_8", "0be58a91-8065-c02b-7a12-2cc14dddbc37");
                AddTexture("map_avatar_you_8", "02fbdc40-5e07-a6e1-228b-58e10f8335b7");
                AddTexture("map_event", "6008be5e-9267-2a3a-9798-e81b076c22ca", "Map event icon");
                AddTexture("map_event_mature", "f9cdba28-a227-d613-2f16-ce06209314ae", "Map mature event icon");
                AddTexture("map_home", "ae9b8f5f-03a1-2e71-db77-6eb27a1ba181", "Map telehub icon");
                AddTexture("map_telehub", "bf1b2bb0-13b1-40ae-3354-b1b93761bdb4", "Map telehub icon");
                AddTexture("map_track_16", "a3878395-ef00-a0e6-ee9a-f45ed6b9ce59");
                AddTexture("map_track_8", "bfdc7bf6-e2ee-1754-f4df-cc25887714ad");

                AddTexture("icon_auction", "96abf5b1-335c-6b76-61e3-74ada07f3cb8");
                AddTexture("icon_avatar_offline", "34648c67-5bfb-5790-e05e-8bd6600fd087");
                AddTexture("icon_avatar_online", "529ed15b-3d41-dcc1-79de-90bf21770b5b");
                AddTexture("icon_day_cycle", "5b30a285-f1e3-92b1-dcd3-0d07366ced3e");
                AddTexture("icon_event", "be235ae0-53cf-1d68-b3ae-cf375ed1fb58");
                AddTexture("icon_event_mature", "cc090999-1b3e-2e97-7a38-c9f4afd10297");
                AddTexture("icon_for_sale", "f20728fd-1670-3771-2293-e0dd3f0bcaab");
                AddTexture("icon_group", "04237108-a879-5a95-9b0c-b18fd09bc447");
                AddTexture("icon_land_for_landless", "c421ddf2-b9d7-b373-503c-f4c423f37f1c");
                AddTexture("icon_place", "ba0bac4e-815e-14e1-2895-5065b8c703b3");
                AddTexture("icon_popular", "bdd47da5-5b5b-c906-37ad-16aaa64f096f");
                AddTexture("icon_top_pick", "77ca91a2-4431-aeaf-6249-3dd99c7dd86d");

                AddTexture("move_backward_in", "db11d956-5e7d-4aa5-b39d-7774d339fc5c");
                AddTexture("move_backward_out", "3ae8bb18-ed97-4cd3-ae5c-d54bc8479fe7");
                AddTexture("move_down_in", "b92a70b9-c841-4c94-b4b3-cee9eb460d48");
                AddTexture("move_down_out", "b5abc9fa-9e62-4e03-bc33-82c4c1b6b689");
                AddTexture("move_forward_in", "54197a61-f5d1-4c29-95d2-c071d08849cb");
                AddTexture("move_forward_out", "a0eb4021-1b20-4a53-892d-8faa9265a6f5");
                AddTexture("move_left_in", "724996f5-b956-46f6-9844-4fcfce1d5e83");
                AddTexture("move_left_out", "82476321-0374-4c26-9567-521535ab4cd7");
                AddTexture("move_right_in", "7eeb57d2-3f37-454d-a729-8b217b8be443");
                AddTexture("move_right_out", "1fbe4e60-0607-44d1-a50a-032eff56ae75");
                AddTexture("move_turn_left_in", "95463c78-aaa6-464d-892d-3a805b6bb7bf");
                AddTexture("move_turn_left_out", "13a93910-6b44-45eb-ad3a-4d1324c59bac");
                AddTexture("move_turn_right_in", "5e616d0d-4335-476f-9977-560bccd009da");
                AddTexture("move_turn_right_out", "5a44fd04-f52b-4c30-8b00-4a31e27614bd");
                AddTexture("move_up_in", "49b4b357-e430-4b56-b9e0-05b8759c3c82");
                AddTexture("move_up_out", "f887146d-829f-4e39-9211-cf872b78f97c");

                AddTexture("bump_bark", "6c9fa78a-1c69-2168-325b-3e03ffa348ce", "Bumpieness");
                AddTexture("bump_bricks", "b8eed5f0-64b7-6e12-b67f-43fa8e773440", "Bumpieness");
                AddTexture("bump_checker", "9deab416-9c63-78d6-d558-9a156f12044c", "Bumpieness");
                AddTexture("bump_concrete", "db9d39ec-a896-c287-1ced-64566217021e", "Bumpieness");
                AddTexture("bump_crustytile", "f2d7b6f6-4200-1e9a-fd5b-96459e950f94", "Bumpieness");
                AddTexture("bump_cutstone", "d9258671-868f-7511-c321-7baef9e948a4", "Bumpieness");
                AddTexture("bump_discs", "d21e44ca-ff1c-a96e-b2ef-c0753426b7d9", "Bumpieness");
                AddTexture("bump_gravel", "4726f13e-bd07-f2fb-feb0-bfa2ac58ab61", "Bumpieness");
                AddTexture("bump_petridish", "e569711a-27c2-aad4-9246-0c910239a179", "Bumpieness");
                AddTexture("bump_siding", "073c9723-540c-5449-cdd4-0e87fdc159e3", "Bumpieness");
                AddTexture("bump_stonetile", "ae874d1a-93ef-54fb-5fd3-eb0cb156afc0", "Bumpieness");
                AddTexture("bump_stucco", "92e66e00-f56f-598a-7997-048aa64cde18", "Bumpieness");
                AddTexture("bump_suction", "83b77fc6-10b4-63ec-4de7-f40629f238c5", "Bumpieness");
                AddTexture("bump_weave", "735198cf-6ea0-2550-e222-21d3c6a341ae", "Bumpieness");
                AddTexture("bump_woodgrain", "058c75c0-a0d5-f2f8-43f3-e9699a89c2fc", "Bumpieness");
                AddTexture("missing_asset", "32dfd1c8-7ff6-5909-d983-6d4adfb4255d", "MISSING IMAGE");
                AddTexture("object_cone", "c2b8c90a-7dca-26e3-1a63-7aa4a0389cf9", "Build tools Cone object type icon");
                AddTexture("object_cone_active", "cf69c64b-f19e-e1f3-a586-42fef31a23be",
                           "Build tools Cone object selected icon");
                AddTexture("object_cube", "70c747ac-1de3-a8b9-514d-101753ca6ccb", "Build tools Cube object type icon");
                AddTexture("object_cube_active", "f9c5e213-1076-7a7d-7889-52388aad2c1a",
                           "Build tools Cube object selected icon");
                AddTexture("object_cylinder", "13e35d95-5f6c-9a91-1766-49dedf9b1267",
                           "Build tools Cylinder object type icon");
                AddTexture("object_cylinder_active", "3f3e4932-8412-e2a7-cfe9-92caf5978b1b",
                           "Build tools Cylinder object selected icon");
                AddTexture("object_grass", "7ca8e672-920b-4653-3970-1abc91abef58", "Build tools Grass object type icon");
                AddTexture("object_grass_active", "d0fc7cc9-646a-6860-cf7c-1d9e58cd6dab",
                           "Build tools Grass object selected icon");
                AddTexture("object_hemi_cone", "69d5e60c-739a-40b1-b526-84072121e394",
                           "Build tools half-cone object type icon");
                AddTexture("object_hemi_cone_active", "2e0c5435-95bb-1c0d-5da1-42336fb1cfc0",
                           "Build tools half-cone object selected icon");
                AddTexture("object_hemi_cylinder", "f4be3e06-24a8-f86e-acc7-7daefc0572b7",
                           "Build tools half-cylinder object type icon");
                AddTexture("object_hemi_cylinder_active", "67279486-cfc1-3633-de42-85db65db373c",
                           "Build tools half-cylinder object selected icon");
                AddTexture("object_hemi_sphere", "b67251ab-1716-b9fb-f911-967ba3fe027b",
                           "Build tools half-sphere object type icon");
                AddTexture("object_hemi_sphere_active", "6c489466-3058-6475-6b1b-e5fc1d49f1f3",
                           "Build tools half-sphere object selected icon");
                AddTexture("object_prism", "02935f3a-dcda-3b42-1874-da89d4c12870", "Build tools Prism object type icon");
                AddTexture("object_prism_active", "223aac97-bd2f-ec2e-ad45-5641b77c78f9",
                           "Build tools Prism object selected icon");
                AddTexture("object_pyramid", "9dde8b56-2cc4-a932-b63a-38c3a83221ad",
                           "Build tools Pyramid object type icon");
                AddTexture("object_pyramid_active", "e7217b1a-e3d8-e339-d28a-d7714d0b5bee",
                           "Build tools Pyramid object selected icon");
                AddTexture("object_ring", "a7610e41-4647-16d8-0e0e-85a1211c1596", "Build tools Ring object type icon");
                AddTexture("object_ring_active", "2c955a73-fa31-237b-a4a1-5c8ede3bae50",
                           "Build tools Ring object selected icon");
                AddTexture("object_sphere", "7fa122c0-b994-460e-8636-cdc451d67268",
                           "Build tools Sphere object type icon");
                AddTexture("object_sphere_active", "f2c3bcbc-2904-41a5-1c22-688f176fd1ee",
                           "Build tools Sphere object selected icon");
                AddTexture("object_tetrahedron", "e17db404-9fc5-9534-1038-777c82b2771f",
                           "Build tools Tetrahedron object type icon");
                AddTexture("object_tetrahedron_active", "2792ea3b-c052-85fe-d168-a62b2f4e9d7c",
                           "Build tools Tetrahedron object selected icon");
                AddTexture("object_torus", "19e1f4c9-6aa6-4414-981d-59a1343a6472", "Build tools Torus object type icon");
                AddTexture("object_torus_active", "ef2bca77-5004-4547-b00a-3b96e463f89f",
                           "Build tools Torus object selected icon");
                AddTexture("object_tree", "710d1bec-fb33-28f1-e77e-ddbb5b51f5ed", "Build tools Tree object type icon");
                AddTexture("object_tree_active", "da4835c7-b12a-41dd-11db-dae452f040c2",
                           "Build tools Tree object selected icon");
                AddTexture("object_tube", "7ce81316-a478-480f-961c-435fcbdecaf0", "Build tools Tube object type icon");
                AddTexture("object_tube_active", "55c3e4d1-cfdc-48a8-af32-a34844b91832",
                           "Build tools Tube object selected icon");
                AddTexture("plywood (default object texture)", "89556747-24cb-43ed-920b-47caed15465f",
                           "The default texture prims are set to upon creation.");
                AddTexture("selected texture", "a85ac674-cb75-4af6-9499-df7c5aaf7a28",
                           "Texture of crosshairs that is overlayed onto the selected texture in *Select Texture* mode.");
                AddTexture("tool_dozer", "d2a0d4d4-54eb-4d16-be4b-4eae43845c74");
                AddTexture("tool_dozer_active", "d4afdbbe-1550-4b7d-91de-95731f47e8e3");
                AddTexture("tool_land", "86fe4df4-0ecb-4382-b9ae-475925a92388");
                AddTexture("tool_land_active", "34e60587-0791-4a07-8918-f5995fcc22a3");
                AddTexture("tool_orbit", "06964fe4-033f-448a-95c9-30dc41d1be8b");
                AddTexture("tool_orbit_active", "ee4e07db-3f72-4098-bd4c-aef34515a7bc");
                AddTexture("tool_pan", "a32aa302-0a15-48d2-b2b1-4d69f1161173");
                AddTexture("tool_pan_active", "24d9ad33-0b42-4eb5-99a3-659d838bc5c0");
                AddTexture("tool_zoom", "27eb8829-fe65-45ed-a49a-73aac42f4b38");
                AddTexture("tool_zoom_active", "69445f58-5c8e-44e0-9d2e-47408bb43b39");
                AddTexture("white", "5748decc-f629-461c-9a36-a35a221fe21f", "The *blank* texture.");


                AddTexture("alpha_noise", "b9e1cf8a-9660-c020-0c69-18f1ea27268a");
                AddTexture("alpha_sizzle", "e121e2fc-7573-740f-edfd-0d45a9ba486e",
                           "Possibly a texture used to generate clouds? Also the texture used for unloaded textures in the texture picker ");
                AddTexture("black", "e2244626-f22f-4839-8123-1e7baddeb659");
                AddTexture("darkgray", "267e26d3-e0e1-41b8-91b1-3b337102928d");
                AddTexture("legend", "ca7609c6-6ec6-32d9-332e-0d8f437ef644");
                AddTexture("lightning", "f8e2c2f0-7d5e-bb9a-68d0-7a3e87984784");
                AddTexture("lightning (flipped horizontally)", "eef34d99-4f5b-efa1-4586-b4853c430dd3");
                AddTexture("lightgray", "c520bf46-cc5d-412b-a60b-9f1bd245189f");
                AddTexture("rounded_square", "38ce8b3c-fb30-5c59-9926-bd643613f606");
                AddTexture("rounded_square_soft", "4c95e6bc-fe77-9cb4-b58a-909848042c1e");


                AddTexture("fringe", "8ac54e9d-ec09-d804-60ab-47404a9b4a36");
                AddTexture("foot_shadow", "14e8a47d-1055-0a68-5d55-eafd9ad3da5b");
                AddTexture("img_smoke_poof", "c734da52-f2ba-f0ba-d59e-15ea49f3d5e9");
                AddTexture("img_shot", "173b05c7-53a9-4cf8-ce6b-5eec21c5c63f");
                AddTexture("folder_arrow", "09a324a8-acc1-d9cd-2cbd-7465d90d3a98");
                AddTexture("color_swatch_alpha", "f13db22f-c55c-8bdf-7b1c-221e56fde253");
                AddTexture("script_error", "e5a0ec29-f59e-d29e-2c59-ed66c187c26c");
                AddTexture("status_script_debug", "7775b5cc-93a5-6efd-0d9b-4e079afac217");
                AddTexture("water_normal", "822ded49-9a6c-f61c-cb89-6df54f42cdf4");
                AddTexture("icon_groupnotice", "21579c81-a85e-f11c-2d80-33a4c007d88c");
                AddTexture("icon_groupnoticeinventory", "8fcca699-08e7-3d58-2f05-86c9d52bbe82");
                AddTexture("tab_background_lightgrey", "c769e547-c307-43ca-2b6a-51cad6d1c527");
                AddTexture("tab_background_purple", "0ae8a2e9-aff4-249c-fc4a-0f41f89847dd");
                AddTexture("tab_background_darkpurple", "38ff4f7e-3078-a749-8302-d6cc94b404c4");
                AddTexture("smicon_warn", "f47c17a3-8bfb-3c9f-22b8-77923de7eed9");
                AddTexture("uv_test1", "f43b75f5-9aa5-18ec-d5d9-e6d1b8442613");
                AddTexture("uv_test2", "300ce95f-3d3f-7c1a-3a22-3fc48f873fb9");
                AddTexture("eye_button_active", "2b42b375-f9b4-788e-46c7-7ef38762d0ba");
                AddTexture("eye_button_inactive", "be1b7225-98b5-eb2a-2c86-ddaae3328a6e");
                AddTexture("account_id_blue", "6ab9179a-7308-58db-6c9d-893d3b52bece");
                AddTexture("account_id_orange", "fbe89371-1251-4e77-d2d8-8eeccffe3ca8");
                AddTexture("account_id_green", "3bf64d5a-38d3-b752-cf52-3d9f8fca353a");
                AddTexture("status_push", "07d1f523-e327-4d10-20d6-8bc22a6e8f56");
                AddTexture("ff_visible_online", "d609a41f-34c0-7aae-b2c6-2fc3ab26d916");
                AddTexture("ff_visible_map", "20b52706-c1ab-414a-9dea-1cb788ad5689");
                AddTexture("ff_edit_mine", "1baee0b9-4b89-39eb-8815-866d82300ab5");
                AddTexture("ff_edit_theirs", "32e981cd-4700-da5a-7fc7-d573ec3742f4");
                AddTexture("inv_item_script_dangerous", "0b502db8-6fcd-c442-ecfe-483a0dce875e");
                AddTexture("ff_visible_map_button", "c1079bef-5cf9-90f3-6dcd-48989851c252");
                AddTexture("ff_visible_online_button", "36749b47-93d6-2c5e-7ebd-d38d30311163");
                AddTexture("ff_edit_theirs_button", "ca229f65-d7e0-133e-1bc2-674abc33f3d5");
                AddTexture("ff_edit_mine_button", "57f05b46-63d8-c3d5-66d6-8b915746b956");
                AddTexture("ff_online_status_button", "3b1b6a53-9c8c-568a-22c5-2a8f3e5286f5");
                AddTexture("oi_hud_cen_0_0", "3c650257-9caf-7cad-b26c-84c9eca560f1");
                AddTexture("oi_hud_intro", "7611fb3d-9ff2-abd3-d98f-805c1c87e757");
                AddTexture("oi_hud_underwater", "cde61aea-83c2-3001-d598-6b348f7a8e0b");
                AddTexture("oi_hud_got_passport", "1271838d-d777-b811-7c4c-2a00308bd80a");
                AddTexture("oi_hud_texture_off_edge", "852be205-b1ea-6356-58c8-8c5ee5a841a6");
                AddTexture("oi_hud_texture_on_edge", "ab11e6ff-a732-be70-67df-c43131274562");
                AddTexture("oi_hud_flyingabovewater", "c9d150d6-2739-5f8b-cce6-3cf98242920a");
                AddTexture("oi_hud_walkingabovewater", "78284eeb-05f3-ff25-11a0-3cc9dbb30f0c");
                AddTexture("oi_hud_landmark", "6cd9c221-9d42-a283-256b-09a113a87271");
                AddTexture("oi_hud_cus_5_3", "7c12f4fb-f502-26d1-a2f3-cdb6aff61663");
                AddTexture("oi_hud_cus_5_2", "c52c9c94-adc0-0f4e-6658-ed33d6ea8829");
                AddTexture("oi_hud_cus_5_1", "9f6d5d11-6ca9-608c-e8a6-b77989350292");
                AddTexture("oi_hud_cus_5_0", "2000cff1-119f-2023-66c0-ac5630d2f96e");
                AddTexture("oi_hud_cus_4_5", "f302a935-ccd1-e2f5-3a38-e185cc262f3a");
                AddTexture("oi_hud_cus_4_3", "af8d5b3c-b40f-cea5-b0b2-440fbd84a11a");
                AddTexture("oi_hud_cus_4_2", "11b26901-8207-12bc-5224-10a12ac4c651");
                AddTexture("oi_hud_cus_4_1", "41baadb7-1b94-907e-9443-54e92bba77cd");
                AddTexture("oi_hud_cus_4_0", "9d627f8e-092c-5d32-6c12-ef76ab81cedc");
                AddTexture("oi_hud_cus_3_4", "b196486e-d0d2-4fd7-529a-c84b4495fc74");
                AddTexture("oi_hud_cus_3_2", "0b81c4bb-de33-e493-7bcb-e7221d97e5e7");
                AddTexture("oi_hud_cus_3_1", "436dab74-25ae-8b60-c648-50663b7faa1d");
                AddTexture("oi_hud_cus_3_0", "6c1594de-1e66-273c-a2ab-8f0ffa8b4633");
                AddTexture("oi_hud_cus_2_4", "bb31fe48-8566-eec0-e96b-64025f832b63");
                AddTexture("oi_hud_cus_2_2", "c946959a-26ae-eb66-efa0-20154057789d");
                AddTexture("oi_hud_cus_2_1", "c946959a-26ae-eb66-efa0-20154057789d");
                AddTexture("oi_hud_cus_2_0", "d7833106-b4a8-7666-bde1-64886de289f9");
                AddTexture("oi_hud_cus_1_0", "811ded22-5940-940c-4821-6fbbfb6611d6");
                AddTexture("oi_hud_cus_1_1", "eda8513b-a343-5109-1fd6-f1c7ad89b703");
                AddTexture("oi_hud_cus_1_2", "7a4ce18c-e715-34d4-dfee-704c270a8ac8");
                AddTexture("oi_hud_cus_1_4", "d3771c15-ac03-b762-b992-d9fd2fedf38a");
                AddTexture("oi_hud_com_4_4", "d9e1e90d-3cc3-6269-128e-67f7a2b32d26");
                AddTexture("oi_hud_com_4_2", "0f649a26-6fdb-c73b-ffac-e50fc311d5ce");
                AddTexture("oi_hud_com_4_1", "ae5b1ce6-a2d2-22d2-f532-6280b3bc6adb");
                AddTexture("oi_hud_com_4_0", "12cda3a0-58c7-dfa8-7f9b-380e5bb8baf9");
                AddTexture("oi_hud_com_3_4", "ff326257-0530-356a-e0f8-be535044e540");
                AddTexture("oi_hud_com_3_2", "66740ddb-1d56-89f9-f0c9-ae5eb7bb9537");
                AddTexture("oi_hud_com_3_1", "55d662f4-6a28-6388-7c75-af1c9fd33055");
                AddTexture("oi_hud_com_3_0", "de9d318f-b69e-82f9-0c61-43b868c5ca6b");
                AddTexture("oi_hud_com_2_4", "01d47e68-400a-d0e1-afb7-d6806d1d477e");
                AddTexture("oi_hud_com_2_0", "09c98850-27d4-6a12-abae-4af4bba23b6b");
                AddTexture("oi_hud_com_1_3", "5c2049b9-f797-6608-ca71-758f3716aa90");
                AddTexture("oi_hud_com_1_1", "1116ff68-cdc4-1cfc-e137-30f8426afeda");
                AddTexture("oi_hud_com_1_0", "bd847d31-f5af-95f7-2b9c-af47d8ba53bd");
                AddTexture("oi_hud_nav_4_5", "66194280-b087-db94-35d9-41e8f7518515");
                AddTexture("oi_hud_nav_4_4", "180c4241-e309-4c05-13ee-9080ab69498d");
                AddTexture("oi_hud_nav_4_3", "e98a6ba6-99c6-fa15-84b6-9afadea6c467");
                AddTexture("oi_hud_nav_4_2", "2e19f352-1893-59a9-949b-4d2cfd3a8222");
                AddTexture("oi_hud_nav_4_1", "13a1675b-fb5a-19b3-b5a3-74b0a6765f7d");
                AddTexture("oi_hud_nav_4_0", "e7526e8d-b085-b26c-b0ae-2708ec231401");
                AddTexture("oi_hud_nav_3_5", "5e67b0d0-29a2-6a08-c85e-b12d59e53d6e");
                AddTexture("oi_hud_nav_3_4", "2ed8fbc2-5c4d-53c2-b289-88baffceab1a");
                AddTexture("oi_hud_nav_3_3", "e0a72f1a-282e-1c1a-2cb7-6423feb41759");
                AddTexture("oi_hud_nav_3_2", "4bcebb23-da5e-47d9-eac1-e4453f762c8c");
                AddTexture("oi_hud_nav_3_1", "6ac87575-330e-3a2d-3b80-a34e7b277e50");
                AddTexture("oi_hud_nav_3_0", "f1451e8e-7310-9152-47d5-5d037c28fef3");
                AddTexture("oi_hud_nav_2_6", "c60b42ff-ee60-98e4-e603-ca2470141d4b");
                AddTexture("oi_hud_nav_2_5", "a02b5a1a-bbdb-5556-ae5b-a2e68494755a");
                AddTexture("oi_hud_nav_2_4", "625535ab-8abf-b3e7-48fb-43f728b77c79");
                AddTexture("oi_hud_nav_2_3", "00a609c3-5750-3b5a-3ce3-458bdf632203");
                AddTexture("oi_hud_nav_2_2", "94903387-d37f-092c-e4d2-c190f68577b8");
                AddTexture("oi_hud_nav_2_1", "ee0cd82c-6ce8-8e73-307b-6d0dc77b19e8");
                AddTexture("oi_hud_nav_2_0", "3e10b379-ed2c-7424-1fe7-bef3558c7536");
                AddTexture("oi_hud_nav_1_4", "bf8d0be8-2012-1664-3ea5-e69a71c206e9");
                AddTexture("oi_hud_nav_1_2", "72100f87-18a7-fc4a-4793-de281e8b02cc");
                AddTexture("oi_hud_nav_1_1", "b048faf3-60ce-c3a2-d034-36613449d377");
                AddTexture("oi_hud_nav_1_0", "0ad45106-3b26-6448-0b90-feae8bd46c38");
                AddTexture("oi_hud_mov_4_5", "7c4a45c2-37dd-312c-c6ab-20896dd0a5a6");
                AddTexture("oi_hud_mov_4_3", "8a88da1c-3735-c71e-d48a-016df0798de4");
                AddTexture("oi_hud_mov_4_2", "f55ae4d3-7d6a-e6ac-4cf7-03014ce14390");
                AddTexture("oi_hud_mov_4_1", "1cc3fcf1-35c0-e222-27d2-6905cf5c4cee");
                AddTexture("oi_hud_mov_4_0", "1ae592dc-46f4-616e-b7c6-0dff3e6f40e5");
                AddTexture("oi_hud_mov_3_4", "831b39be-99fc-45bd-ba85-708f9dc93bfd");
                AddTexture("oi_hud_mov_3_2", "9f7e7373-92a9-d66a-ad5a-afb55ca6ac1f");
                AddTexture("oi_hud_mov_3_1", "ab37ed0d-7e66-1f77-3acf-b0fe4b74dbe8");
                AddTexture("oi_hud_mov_3_0", "f5ff1f08-4c92-8606-1854-cc5b9d3e445c");
                AddTexture("oi_hud_mov_1_2", "1e3abeed-e893-c44e-1f9d-5ecc76d21e5d");
                AddTexture("oi_hud_mov_1_0", "e300fc95-aa94-8e31-c501-ce903cac8b7c");

                AddTexture("numbers_1", "1494e996-a91b-f770-bd10-ad788642d859");
                AddTexture("numbers_2", "f545e486-2a2e-730d-845a-cbe9d4bcb9fd");
                AddTexture("numbers_3", "16a84092-421f-3225-bb36-4071b55fab2d");
                AddTexture("numbers_4", "4b0a62c4-65f4-932e-d440-7fe3cf5a1540");
                AddTexture("numbers_5", "e0e7eacd-e956-14e6-df65-3bc3b7d4e679");
                AddTexture("numbers_6", "6f579c89-bf1f-71d7-854d-b08341edf51e");
                AddTexture("numbers_7", "a5063c9b-377c-1244-62b4-7ce5d1dbfafe");
                AddTexture("numbers_8", "b055ba6a-d2ff-d67d-066c-ccb8a9e8300d");
                AddTexture("numbers_9", "42b58e86-e83c-bc1b-4d73-ab3603d00d98");
                AddTexture("numbers_0", "872560f2-d964-ab22-d558-1607366666c1");
                AddTexture("hearts", "fcdac14d-6128-ce47-66bc-a3d0d27f6d3d");
                AddTexture("circle.tga", "0498c309-5306-43cd-82a2-ae31d096cdef");
                AddTexture("mute_icon.tga", "37c8e000-6aa2-41ef-8f86-e0c2e60bfa42");
                AddTexture("active_speakers.tga", "c97bdfb5-b0da-4741-877c-7c1553957d30");
                AddTexture("active_voice_tab.tga", "33281629-74b3-4b0e-98e7-a6383eb277fa");
                AddTexture("music_icon.tga", "9de3ef3d-ab90-4963-be15-ae77a122a484");
                AddTexture("media_icon.tga", "9724ad2b-b0ec-4b8c-9558-73f36661db26");
                AddTexture("volume_icon.tga", "eb510e2b-3815-4434-b502-b18712db65cc");
                AddTexture("icn_active-speakers-dot-lvl0.tga", "73577b7b-19c3-4050-a19d-36bc2408aa79");
                AddTexture("icn_active-speakers-dot-lvl1.tga", "8f761ce3-5939-4d3a-8991-00064fdfacf9");
                AddTexture("icn_active-speakers-dot-lvl2.tga", "0e82d24e-ed45-41bc-b090-94c97c1caab2");
                AddTexture("icn_active-speakers-typing1.tga", "f9bbb2fe-584b-4c01-86fc-599c69534c1b");
                AddTexture("icn_active-speakers-typing2.tga", "e3369e02-93e1-43dc-b9c0-4533db0963d0");
                AddTexture("icn_active-speakers-typing3.tga", "13dd1d96-6836-461e-8a4c-36003065c59b");
                AddTexture("icn_voice_ptt-off.tga", "ce19b99f-bd2d-4324-88ab-975c357f9e4e");
                AddTexture("icn_voice_ptt-on.tga", "67f672e1-576f-42ee-973f-c796cc8eefb1");
                AddTexture("icn_voice_ptt-on-lvl1.tga", "24ebff50-ced7-4d52-8b74-77a5a901e701");
                AddTexture("icn_voice_ptt-on-lvl2.tga", "1a803501-0da7-4b80-80db-1f79a7842368");
                AddTexture("icn_voice_ptt-on-lvl3.tga", "3342bfc4-0bd9-4c89-8e87-e5980aee00e5");

                AddAThuruZ(new string[]
                               {
                                   "05fb8c9a-2ace-8c64-6688-9a08f535e87b",
                                   "c68b3416-1b02-76f4-8295-5170f2a430fc",
                                   "ab55d18b-f220-7dad-2036-9dbe6d9d24ae",
                                   "bb14e17f-48e2-b0de-2d3a-f3fc919d3455",
                                   "81d319dc-08a1-70ff-7d08-d2d5e8521d90",
                                   "993b9e32-77cd-17c6-b9d5-aa0950eeeedc",
                                   "4bbbec6c-26c7-b28c-8e2b-b95fd6b4c73e",
                                   "5c52c7da-7443-8d03-1493-94a167981f74",
                                   "7bfad861-8374-84fb-f71f-2a58fb6dd64a",
                                   "e79f9242-17d9-1377-f8b1-59812df58def",
                                   "6121cbf7-45dd-81d1-5983-bb4da8bb6cbd",
                                   "b9c2b4f6-2dd2-f05e-2ebe-8291a1c8208c",
                                   "e4c16f2e-5589-4dab-d658-2d8f8e78e212",
                                   "3dd9c930-88cb-80cd-04df-a146429166ba",
                                   "aa61ecc4-59c6-d390-b0e2-770abd463037",
                                   "702072b1-a14a-c1c2-7e96-ec69044989ef",
                                   "ed6b8c52-2540-df1e-2fb1-5c2d74d52951",
                                   "50ceb1e2-bf65-43d1-14b9-f7c6baab856f",
                                   "d1684b98-25c3-6d6d-9517-cc5b15264a5d",
                                   "d2daf814-6aa7-7a59-54b2-3110bf0d9dee",
                                   "367def79-30ef-5de7-cade-55fb1973dec4",
                                   "02e99a90-31c8-b50b-8b16-2c182d67a2d4",
                                   "722b77a3-8513-9537-c0cb-00978c85745e",
                                   "ea2aa90d-4a98-8d3b-3815-4505113a5a90",
                                   "ebe59dea-4a23-d415-1503-9c789c68d0f5",
                                   "9fbdeb14-7814-2c21-ab27-ee1867e349a3"
                               });
                AddTextureR("701917a8-d614-471f-13dd-5f4644e36e3c", "Space");
                AddTextureR("d3f39664-ec05-92f0-1b45-db3acfe276e3", "Period");
                AddTextureR("3ff82ed4-c521-dc69-b1ad-5d70f4b89da1", "Question");
                AddTextureR("eaa3ec1a-d071-285e-7058-14ce838e8e25", "Semicolon");
                AddTextureR("c0eeefcc-fa2c-0ff9-c9b9-babc365643ea", "Exclamation");
                AddTextureR("8ccd81a4-ac36-7618-d7b4-b40fc2a548f0", "Comma");
                AddTextureR("f81176b3-32b8-86bf-3613-a9f89895c1ad", "Colon");



                AddSound("bell ting", "ed124764-705d-d497-167a-182cd9fa2e6c");
                AddSound("click", "4c8c3c77-de8d-bde2-b9b8-32635e0fd4a6");
                AddSound("health reduction (female)", "219c5d93-6c09-31c5-fb3f-c5fe7495c115");
                AddSound("health reduction (male)", "e057c244-5768-1056-c37e-1537454eeb62");
                AddSound("IM session, new incoming (2 bells)", "67cc2844-00f3-2b3c-b991-6418d01e1bb7");
                AddSound("IM start", "c825dfbc-9827-7e02-6507-3713d18916c1");
                AddSound("instant message notification", "67cc2844-00f3-2b3c-b991-6418d01e1bb7");
                AddSound("invalid operation", "4174f859-0d3d-c517-c424-72923dc21f65");
                AddSound("keyboard loop", "5e191c7b-8996-9ced-a177-b2ac32bfea06");
                AddSound("money change, down (coins)", "77a018af-098e-c037-51a6-178f05877c6f");
                AddSound("money change, up (cash register bell)", "104974e3-dfda-428b-99ee-b0d4e748d3a3");
                AddSound("null keystroke", "2ca849ba-2885-4bc3-90ef-d4987a5b983a");
                AddSound("object collision", "be582e5d-b123-41a2-a150-454c39e961c8");
                AddSound("object collision (rubber)", "212b6d1e-8d9c-4986-b3aa-f3c6df8d987d");
                AddSound("object collision (plastic)", "d55c7f3c-e1c3-4ddc-9eff-9ef805d9190e");
                AddSound("object collision (flesh)", "2d8c6f51-149e-4e23-8413-93a379b42b67");
                AddSound("object collision (wood splintering?)", "6f00669f-15e0-4793-a63e-c03f62fee43a");
                AddSound("object collision (glass break)", "85cda060-b393-48e6-81c8-2cfdfb275351");
                AddSound("object collision (metal clunk)", "d1375446-1c4d-470b-9135-30132433b678");
                AddSound("object create (whoosh)", "f4a0660f-5446-dea2-80b7-6482a082803c");
                AddSound("object delete", "0cb7b00a-4c10-6948-84de-a93c09af2ba9");
                AddSound("object rez (shake)", "3c8fc726-1fd6-862d-fa01-16c5b2568db6");
                AddSound("pie menu appear (ding)", "8eaed61f-92ff-6485-de83-4dcc938a478e");
                AddSound("pie menu slice highlight", "d9f73cf8-17b4-6f7a-1565-7951226c305d");
                AddSound("pie menu slice highlight1", "f6ba9816-dcaf-f755-7b67-51b31b6233e5");
                AddSound("pie menu slice highlight2", "7aff2265-d05b-8b72-63c7-dbf96dc2f21f");
                AddSound("pie menu slice highlight3", "09b2184e-8601-44e2-afbb-ce37434b8ba1");
                AddSound("pie menu slice highlight4", "bbe4c7fc-7044-b05e-7b89-36924a67593c");
                AddSound("pie menu slice highlight5", "d166039b-b4f5-c2ec-4911-c85c727b016c");
                AddSound("pie menu slice highlight6", "242af82b-43c2-9a3b-e108-3b0c7e384981");
                AddSound("pie menu slice highlight7", "c1f334fb-a5be-8fe7-22b3-29631c21cf0b");
                AddSound("snapshot", "3d09f582-3851-c0e0-f5ba-277ac5c73fb4");
                AddSound("teleport/texture apply", "d7a9a565-a013-2a69-797d-5332baa1a947");
                AddSound("thunder", "e95c96a5-293c-bb7a-57ad-ce2e785ad85f");
                AddSound("window close", "2c346eda-b60c-ab33-1119-b8941916a499");
                AddSound("window open (same as above)", "c80260ba-41fd-8a46-768a-6bf236360e3a");
                AddSound("zipper? (Floor squeek)", "6cf2be26-90cb-2669-a599-f5ab7698225f");

                if (WorldObjects.MaintainAssetsInFolders && Directory.Exists("sound_files/"))
                {
                    foreach (string files in Directory.GetFiles("sound_files/"))
                    {
                        if (!files.EndsWith("sound")) continue;
                        byte[] bs = File.ReadAllBytes(files);
                        string name = Path.GetFileNameWithoutExtension(Path.GetFileName(files)).ToLower();
                        if (nameAsset.ContainsKey(name)) continue;
                        WriteLine("Sound w/o UUID " + name);
                        SimSound sound = new SimSound(UUID.Zero, name, AssetType.Sound);
                        nameAsset[name] = sound;
                    }
                }


               // if (uuidAsset.Count > 0) return;
                /*
                 * need UUIDS for
                        customize.animation
                        customize_done.animation
                        express_disdain.animation
                        express_frown.animation
                        express_kiss.animation
                        express_open_mouth.animation
                        express_smile.animation
                        express_tongue_out.animation
                        express_toothsmile.animation
                 */
                AnimUUID("AIM_L_BOW", "46bb4359-de38-4ed8-6a22-f1f52fe8f506");
                AnimUUID("AIM_R_BAZOOKA", "b5b4a67d-0aee-30d2-72cd-77b333e932ef");
                AnimUUID("AIM_R_HANDGUN", "3147d815-6338-b932-f011-16b56d9ac18b");
                AnimUUID("AIM_R_RIFLE", "ea633413-8006-180a-c3ba-96dd1d756720");
                AnimUUID("ANGRY_FINGERWAG", "c1bc7f36-3ba0-d844-f93c-93be945d644f");
                AnimUUID("ANGRY_TANTRUM", "11000694-3f41-adc2-606b-eee1d66f3724");
                AnimUUID("AWAY", "fd037134-85d4-f241-72c6-4f42164fedee");
                AnimUUID("BACKFLIP", "c4ca6188-9127-4f31-0158-23c4e2f93304");
                AnimUUID("BLOWKISS", "db84829b-462c-ee83-1e27-9bbee66bd624");
                AnimUUID("BOW", "82e99230-c906-1403-4d9c-3889dd98daba");
                AnimUUID("BRUSH", "349a3801-54f9-bf2c-3bd0-1ac89772af01");
                AnimUUID("BUSY", "efcf670c-2d18-8128-973a-034ebc806b67");
                AnimUUID("CLAP", "9b0c1c4e-8ac7-7969-1494-28c874c4f668");
                AnimUUID("COURTBOW", "9ba1c942-08be-e43a-fb29-16ad440efc50");
                AnimUUID("CROUCH", "Crouching", "201f3fdf-cb1f-dbec-201f-7333e328ae7c");
                AnimUUID("CROUCHWALK", "CrouchWalking", "47f5f6fb-22e5-ae44-f871-73aaaf4a6022");
                AnimUUID("DANCE1", "b68a3d7c-de9e-fc87-eec8-543d787e5b0d");
                AnimUUID("DANCE2", "928cae18-e31d-76fd-9cc9-2f55160ff818");
                AnimUUID("DANCE3", "30047778-10ea-1af7-6881-4db7a3a5a114");
                AnimUUID("DANCE4", "951469f4-c7b2-c818-9dee-ad7eea8c30b7");
                AnimUUID("DANCE5", "4bd69a1d-1114-a0b4-625f-84e0a5237155");
                AnimUUID("DANCE6", "cd28b69b-9c95-bb78-3f94-8d605ff1bb12");
                AnimUUID("DANCE7", "a54d8ee2-28bb-80a9-7f0c-7afbbe24a5d6");
                AnimUUID("DANCE8", "b0dc417c-1f11-af36-2e80-7e7489fa7cdc");
                AnimUUID("DEAD", "57abaae6-1d17-7b1b-5f98-6d11a6411276");
                AnimUUID("DRINK", "0f86e355-dd31-a61c-fdb0-3a96b9aad05f");
                AnimUUID("EXPRESS_AFRAID", "6b61c8e8-4747-0d75-12d7-e49ff207a4ca");
                AnimUUID("EXPRESS_AFRAID_EMOTE", "aa2df84d-cf8f-7218-527b-424a52de766e");
                AnimUUID("EXPRESS_ANGER", "5747a48e-073e-c331-f6f3-7c2149613d3e");
                AnimUUID("EXPRESS_ANGER_EMOTE", "1a03b575-9634-b62a-5767-3a679e81f4de");
                AnimUUID("EXPRESS_BORED", "b906c4ba-703b-1940-32a3-0c7f7d791510");
                AnimUUID("EXPRESS_BORED_EMOTE", "214aa6c1-ba6a-4578-f27c-ce7688f61d0d");
                AnimUUID("EXPRESS_CRY", "92624d3e-1068-f1aa-a5ec-8244585193ed");
                AnimUUID("EXPRESS_CRY_EMOTE", "d535471b-85bf-3b4d-a542-93bea4f59d33");
                AnimUUID("EXPRESS_DISDAIN", "d4416ff1-09d3-300f-4183-1b68a19b9fc1");
                AnimUUID("EXPRESS_EMBARRASED", "514af488-9051-044a-b3fc-d4dbf76377c6");
                AnimUUID("EXPRESS_EMBARRASSED_EMOTE", "0b8c8211-d78c-33e8-fa28-c51a9594e424");
                AnimUUID("EXPRESS_FROWN", "fee3df48-fa3d-1015-1e26-a205810e3001");
                AnimUUID("EXPRESS_KISS", "1e8d90cc-a84e-e135-884c-7c82c8b03a14");
                AnimUUID("EXPRESS_LAUGH", "18b3a4b5-b463-bd48-e4b6-71eaac76c515");
                AnimUUID("EXPRESS_LAUGH_EMOTE", "62570842-0950-96f8-341c-809e65110823");
                AnimUUID("EXPRESS_OPEN_MOUTH", "d63bc1f9-fc81-9625-a0c6-007176d82eb7");
                AnimUUID("EXPRESS_REPULSED", "36f81a92-f076-5893-dc4b-7c3795e487cf");
                AnimUUID("EXPRESS_REPULSED_EMOTE", "f76cda94-41d4-a229-2872-e0296e58afe1");
                AnimUUID("EXPRESS_SAD", "0eb702e2-cc5a-9a88-56a5-661a55c0676a");
                AnimUUID("EXPRESS_SAD_EMOTE", "eb6ebfb2-a4b3-a19c-d388-4dd5c03823f7");
                AnimUUID("EXPRESS_SHRUG", "70ea714f-3a97-d742-1b01-590a8fcd1db5");
                AnimUUID("EXPRESS_SHRUG_EMOTE", "a351b1bc-cc94-aac2-7bea-a7e6ebad15ef");
                AnimUUID("EXPRESS_SMILE", "b7c7c833-e3d3-c4e3-9fc0-131237446312");
                AnimUUID("EXPRESS_SURPRISE", "313b9881-4302-73c0-c7d0-0e7a36b6c224");
                AnimUUID("EXPRESS_SURPRISE_EMOTE", "728646d9-cc79-08b2-32d6-937f0a835c24");
                AnimUUID("EXPRESS_TOOTHSMILE", "b92ec1a5-e7ce-a76b-2b05-bcdb9311417e");
                AnimUUID("EXPRESS_TONGUE_OUT", "835965c6-7f2f-bda2-5deb-2478737f91bf");
                AnimUUID("EXPRESS_WINK", "869ecdad-a44b-671e-3266-56aef2e3ac2e");
                AnimUUID("EXPRESS_WINK_EMOTE", "da020525-4d94-59d6-23d7-81fdebf33148");
                AnimUUID("EXPRESS_WORRY", "9f496bd2-589a-709f-16cc-69bf7df1d36c");
                AnimUUID("EXPRESS_WORRY_EMOTE", "9c05e5c7-6f07-6ca4-ed5a-b230390c3950");
                AnimUUID("FALLDOWN", "Falling", "666307d9-a860-572d-6fd4-c3ab8865c094");
                AnimUUID("FEMALE_WALK", "Walking", "f5fc7433-043d-e819-8298-f519a119b688");
                AnimUUID("FIST_PUMP", "7db00ccd-f380-f3ee-439d-61968ec69c8a");
                AnimUUID("FLY", "Flying", "aec4610c-757f-bc4e-c092-c6e9caf18daf");
                AnimUUID("FLYSLOW", "FlyingSlow", "2b5a38b2-5e00-3a97-a495-4c826bc443e6");
                AnimUUID("HELLO", "9b29cd61-c45b-5689-ded2-91756b8d76a9");
                AnimUUID("HOLD_L_BOW", "8b102617-bcba-037b-86c1-b76219f90c88");
                AnimUUID("HOLD_R_BAZOOKA", "ef62d355-c815-4816-2474-b1acc21094a6");
                AnimUUID("HOLD_R_HANDGUN", "efdc1727-8b8a-c800-4077-975fc27ee2f2");
                AnimUUID("HOLD_R_RIFLE", "3d94bad0-c55b-7dcc-8763-033c59405d33");
                AnimUUID("HOLD_THROW_R", "7570c7b5-1f22-56dd-56ef-a9168241bbb6");
                AnimUUID("HOVER", "Hovering", "4ae8016b-31b9-03bb-c401-b1ea941db41d");
                AnimUUID("HOVER_DOWN", "Hovering Down", "20f063ea-8306-2562-0b07-5c853b37b31e");
                AnimUUID("HOVER_UP", "Hovering Up", "62c5de58-cb33-5743-3d07-9e4cd4352864");
                AnimUUID("IMPATIENT", "5ea3991f-c293-392e-6860-91dfa01278a3");
                AnimUUID("JUMP", "Jumping", "2305bd75-1ca9-b03b-1faa-b176b8a8c49e");
                AnimUUID("JUMPFORJOY", "709ea28e-1573-c023-8bf8-520c8bc637fa");
                AnimUUID("KICK_ROUNDHOUSE_R", "49aea43b-5ac3-8a44-b595-96100af0beda");
                AnimUUID("KISSMYBUTT", "19999406-3a3a-d58c-a2ac-d72e555dcf51");
                AnimUUID("LAND", "Landing", "7a17b059-12b2-41b1-570a-186368b6aa6f");
                AnimUUID("LAUGH_SHORT", "ca5b3f14-3194-7a2b-c894-aa699b718d1f");
                AnimUUID("MOTORCYCLE_SIT", "08464f78-3a8e-2944-cba5-0c94aff3af29");
                AnimUUID("MUSCLEBEACH", "315c3a41-a5f3-0ba4-27da-f893f769e69b");
                AnimUUID("NO_HEAD", "5a977ed9-7f72-44e9-4c4c-6e913df8ae74");
                AnimUUID("NO_UNHAPPY", "d83fa0e5-97ed-7eb2-e798-7bd006215cb4");
                AnimUUID("NYANYA", "f061723d-0a18-754f-66ee-29a44795a32f");
                AnimUUID("PEACE", "b312b10e-65ab-a0a4-8b3c-1326ea8e3ed9");
                AnimUUID("POINT_ME", "17c024cc-eef2-f6a0-3527-9869876d7752");
                AnimUUID("POINT_YOU", "ec952cca-61ef-aa3b-2789-4d1344f016de");
                AnimUUID("PREJUMP", "PreJumping", "7a4e87fe-de39-6fcb-6223-024b00893244");
                AnimUUID("PUNCH_L", "f3300ad9-3462-1d07-2044-0fef80062da0");
                AnimUUID("PUNCH_ONETWO", "eefc79be-daae-a239-8c04-890f5d23654a");
                AnimUUID("PUNCH_R", "c8e42d32-7310-6906-c903-cab5d4a34656");
                AnimUUID("RPS_COUNTDOWN", "35db4f7e-28c2-6679-cea9-3ee108f7fc7f");
                AnimUUID("RPS_PAPER", "0836b67f-7f7b-f37b-c00a-460dc1521f5a");
                AnimUUID("RPS_ROCK", "42dd95d5-0bc6-6392-f650-777304946c0f");
                AnimUUID("RPS_SCISSORS", "16803a9f-5140-e042-4d7b-d28ba247c325");
                AnimUUID("RUN", "Running", "05ddbff8-aaa9-92a1-2b74-8fe77a29b445");
                AnimUUID("SALUTE", "cd7668a6-7011-d7e2-ead8-fc69eff1a104");
                AnimUUID("SHOOT_L_BOW", "e04d450d-fdb5-0432-fd68-818aaf5935f8");
                AnimUUID("SHOUT", "6bd01860-4ebd-127a-bb3d-d1427e8e0c42");
                AnimUUID("SIT", "Sitting", "1a5fe8ac-a804-8a5d-7cbd-56bd83184568");
                AnimUUID("SIT_FEMALE", "Sitting", "b1709c8d-ecd3-54a1-4f28-d55ac0840782");
                AnimUUID("SIT_GENERIC", "Sitting", "245f3c54-f1c0-bf2e-811f-46d8eeb386e7");
                AnimUUID("SIT_GROUND", "Sitting on Ground", "1c7600d6-661f-b87b-efe2-d7421eb93c86");
                AnimUUID("SIT_GROUND_CONSTRAINED", "Sitting on Ground", "1a2bd58e-87ff-0df8-0b4c-53e047b0bb6e");
                AnimUUID("SIT_TO_STAND", "a8dee56f-2eae-9e7a-05a2-6fb92b97e21e");
                AnimUUID("SLEEP", "f2bed5f9-9d44-39af-b0cd-257b2a17fe40");
                AnimUUID("SMOKE_IDLE", "d2f2ee58-8ad1-06c9-d8d3-3827ba31567a");
                AnimUUID("SMOKE_INHALE", "6802d553-49da-0778-9f85-1599a2266526");
                AnimUUID("SMOKE_THROW_DOWN", "0a9fb970-8b44-9114-d3a9-bf69cfe804d6");
                AnimUUID("SNAPSHOT", "eae8905b-271a-99e2-4c0e-31106afd100c");
                AnimUUID("SOFT_LAND", "Soft Landing", "f4f00d6e-b9fe-9292-f4cb-0ae06ea58d57");
                AnimUUID("STAND", "Standing", "2408fe9e-df1d-1d7d-f4ff-1384fa7b350f");
                AnimUUID("STAND_1", "Standing", "15468e00-3400-bb66-cecc-646d7c14458e");
                AnimUUID("STAND_2", "Standing", "370f3a20-6ca6-9971-848c-9a01bc42ae3c");
                AnimUUID("STAND_3", "Standing", "42b46214-4b44-79ae-deb8-0df61424ff4b");
                AnimUUID("STAND_4", "Standing", "f22fed8b-a5ed-2c93-64d5-bdd8b93c889f");
                AnimUUID("STANDUP", "Standing Up", "3da1d753-028a-5446-24f3-9c9b856d9422");
                AnimUUID("STRETCH", "80700431-74ec-a008-14f8-77575e73693f");
                AnimUUID("STRIDE", "Striding", "1cb562b0-ba21-2202-efb3-30f82cdf9595");
                AnimUUID("SURF", "41426836-7437-7e89-025d-0aa4d10f1d69");
                AnimUUID("SWORD_STRIKE_R", "85428680-6bf9-3e64-b489-6f81087c24bd");
                AnimUUID("TALK", "5c682a95-6da4-a463-0bf6-0f5b7be129d1");
                AnimUUID("THROW_R", "aa134404-7dac-7aca-2cba-435f9db875ca");
                AnimUUID("TRYON_SHIRT", "83ff59fe-2346-f236-9009-4e3608af64c1");
                AnimUUID("TURN_180", "038fcec9-5ebd-8a8e-0e2e-6e71a0a1ac53");
                AnimUUID("TURNBACK_180", "6883a61a-b27b-5914-a61e-dda118a9ee2c");
                AnimUUID("TURNLEFT", "Turning Left", "56e0ba0d-4a9f-7f27-6117-32f2ebbf6135");
                AnimUUID("TURNRIGHT", "Turning Right", "2d6daa51-3192-6794-8e2e-a15f8338ec30");
                AnimUUID("TYPE", "c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
                AnimUUID("WALK", "Walking", "6ed24bd8-91aa-4b12-ccc7-c97c857ab4e0");
                AnimUUID("WHISPER", "7693f268-06c7-ea71-fa21-2b30d6533f8f");
                AnimUUID("WHISTLE", "b1ed7982-c68e-a982-7561-52a88a5298c0");
                AnimUUID("WINK_HOLLYWOOD", "c0c4030f-c02b-49de-24ba-2331f43fe41c");
                AnimUUID("YES_HAPPY", "b8c8b2a3-9008-1771-3bfc-90924955ab2d");
                AnimUUID("YES_HEAD", "15dd911d-be82-2856-26db-27659b142875");
                AnimUUID("YOGA_FLOAT", "42ecd00b-9947-a97c-400a-bbc9174c7aeb");
                AnimUUID("DO_CRUCIFIX_BOOK", "6a492cee-d738-455c-7042-6ef1e52c8ad4");
                AnimUUID("WRITING", "25a57306-f2b8-4821-a28e-23bac5eb5c4a");
                AnimUUID("TYPING", "944daa15-4be4-4319-9568-14ce5d1fcf01");


                // from http://wiki.secondlife.com/wiki/Internal_Animations#Viewer-generated_motions
                AnimUUID("body_noise", "9aa8b0a6-0c6f-9518-c7c3-4f41f2c001ad", "LLBodyNoiseMotion in newview/llvoavatar.cpp", "2", "Minor body turns. Joints moved: torso.");
                AnimUUID("breathe_rot", "4c5a103e-b830-2f1c-16bc-224aa0ad5bc8", "LLBreatheMotionRot in newview/llvoavatar.cpp", "1", "Breathing simulation. Joints moved: chest");
                AnimUUID("editing", "2a8eba1d-a7f8-5596-d44a-b4977bf8c8bb", "LLEditingMotion in llcharacter/lleditingmotion.cpp", "2", "Left arm follows selected and edited objects. Joints moved: left shoulder, left elbow, left wrist, torso.");
                AnimUUID("eye", "5c780ea8-1cd1-c463-a128-48c023f6fbea", "LLEyeMotion in llcharacter/llheadrotmotion.cpp", "1", "Eye rotation and blinking. Joints moved: head, left eye, right eye");
                AnimUUID("fly_adjust", "db95561f-f1b0-9f9a-7224-b12f71af126e", "LLFlyAdjustMotion in llcharacter/llkeyframewalkmotion.cpp", "3", "Add body roll during flight. Joints moved: pelvis");
                AnimUUID("hand_motion", "ce986325-0ba7-6e6e-cc24-b17c4b795578", "LLHandMotion in llcharacter/llhandmotion.cpp", "1", "Sets standard hand poses defined in other animations");
                AnimUUID("head_rot", "e6e8d1dd-e643-fff7-b238-c6b4b056a68d", "LLHeadRotMotion in llcharacter/llheadrotmotion.cpp", "1", "Moves head and torso to follow the avatar's look at position (cursor, camera). Joints moved: torso, neck, head");
                AnimUUID("pelvis_fix", "0c5dd2a2-514d-8893-d44d-05beffad208b", "LLPelvisFixMotion in newview/llvoavatar.cpp", "0", "Makes corrections to keep avatar standing upright. Joints moved: pelvis");
                AnimUUID("target", "0e4896cb-fba4-926c-f355-8720189d5b55", "LLTargetingMotion in llcharacter/lltargetingmotion.cpp", "2", "Move body with look at position, used during aim_* animations above. Joints moved: pelvis, torso, right wrist");
                AnimUUID("walk_adjust", "829bc85b-02fc-ec41-be2e-74cc6dd7215d", "LLWalkAdjustMotion in llcharacter/llkeyframewalkmotion.cpp", "2", "Makes walking corrections for terrain, turns. Joints moved: pelvis, left ankle, right ankle");
                
                //common extras i've noticed
                AnimUUID("drinking_or_snowcone", "758547a2-7212-d533-43e3-1666eda1705e");

                AddAssetAlias("aim_bazooka_r", "avatar_aim_R_bazooka");
                AddAssetAlias("aim_bow_l", "avatar_aim_L_bow");
                AddAssetAlias("aim_handgun_r", "avatar_aim_R_handgun");
                AddAssetAlias("aim_rifle_r", "avatar_aim_R_rifle");
                AddAssetAlias("medium_land", "avatar_soft_land");
                AddAssetAlias("muscle_beach", "avatar_musclebeach");
                AddAssetAlias("no", "avatar_no_head");
                AddAssetAlias("nyah_nyah", "avatar_nyanya");
                AddAssetAlias("onetwo_punch", "avatar_punch_onetwo");
                AddAssetAlias("pre_jump", "avatar_prejump");
                AddAssetAlias("punch_left", "avatar_punch_L");
                AddAssetAlias("punch_right", "avatar_punch_R");
                AddAssetAlias("roundhouse_kick", "avatar_kick_roundhouse_R");
                AddAssetAlias("shoot_bow_l", "avatar_shoot_L_bow");
                AddAssetAlias("sit_ground_staticrained", "avatar_sit_ground_constrained");
                AddAssetAlias("sword_strike", "avatar_sword_strike_R");
                AddAssetAlias("tantrum", "avatar_angry_tantrum");
                AddAssetAlias("yes", "avatar_yes_head");
                AddAssetAlias("blow_kiss", "avatar_blowkiss");
                AddAssetAlias("busy", "avatar_away");
                AddAssetAlias("finger_wag", "avatar_angry_fingerwag");
                AddAssetAlias("hold_bazooka_r", "avatar_hold_R_bazooka");
                AddAssetAlias("hold_bow_l", "avatar_hold_L_bow");
                AddAssetAlias("hold_handgun_r", "avatar_hold_R_handgun");
                AddAssetAlias("hold_rifle_r", "avatar_hold_R_rifle");
                AddAssetAlias("jump_for_joy", "avatar_jumpforjoy");
                AddAssetAlias("kiss_my_butt", "avatar_kissmybutt");
                // TODO: these animations need corrected likely
                AddAssetAlias("express_embarrassed", "avatar_express_embarrased");
                AddAssetAlias("embarrassed", "avatar_express_embarrased");
                AddAssetAlias("belly_laugh", "avatar_express_laugh");
                AddAssetAlias("angry", "avatar_angry_tantrum");

                foreach (FieldInfo fi in typeof(Animations).GetFields())
                {
                    AnimUUID(fi.Name, ((UUID)fi.GetValue(null)).ToString());
                    ////string uids = uid.ToString();
                    //SetAnimationName(uid, fName);
                    ////                    uuidAnim[uid] = fName;
                    //WorldObjects.RegisterUUID(uid, fName);
                    //                  nameAnim[fName] = uid;
                }
                if (WorldObjects.MaintainAnimsInFolders && Directory.Exists("bvh_files/"))
                {
                    foreach (string files in Directory.GetFiles("bvh_files/"))
                    {
                        if (!files.EndsWith("animation")) continue;
                        byte[] bs = File.ReadAllBytes(files);
                        string name = Path.GetFileNameWithoutExtension(Path.GetFileName(files)).ToLower();               
                        if (nameAsset.ContainsKey(name)) continue;
                        UUID uuid = TheStore.GetAssetUUID(name, AssetType.Animation);
                        SimAsset anim;
                        if (uuid==UUID.Zero)
                        {
                            WriteLine("Anim w/o UUID " + name);
                            anim = new SimAnimation(UUID.Zero, name);                            
                        } else
                        {
                            anim = FindAsset(uuid);
                        }
                        if (anim != null)
                        {
                            nameAsset[name] = anim;
                        } else
                        {
                            WriteLine("NULL Anim w/o UUID " + name);  
                        }
                    }
                }
                if (File.Exists("AssetMapping.xml"))
                {
                    LoadAssetFile("AssetMapping.xml");
                }
                SimAnimation.ClassifyAnims();
#if SPAMMY_DEBUG
                //lock (SimAssets) 
                    foreach (SimAsset A in SimAssets)
                    {
                        if (A.IsIncomplete()) WriteLine("Incomplete Asset: " + A.ToString());
                    }
#endif
            }
            
        }

        public static void LoadAssetFile(string s)
        {
            try
            {
                XmlDocument doc = new XmlDocument();
                doc.Load(s);
                foreach (XmlNode node in doc.DocumentElement.ChildNodes)
                {
                    string nodename = node.Name.ToLower();
                    if (nodename == "alias")
                    {
                        string type = GetAttribValue(node, "type", String.Empty);
                        string to = GetAttribValue(node, "to", String.Empty);
                        string from = GetAttribValue(node, "from", String.Empty);
                        AssetType atype = AssetType.Unknown;
                        try
                        {
                            try
                            {
                                atype = (AssetType)Enum.Parse(typeof(AssetType), type.Substring(0, 1).ToUpper() + type.Substring(1).ToLower());
                            }
                            catch (Exception)
                            {
                                atype = (AssetType)Enum.Parse(typeof(AssetType), type);
                            }
                        }
                        catch (Exception)
                        {
                            try
                            {
                                atype = (AssetType)Enum.Parse(typeof(AssetType), type.ToUpper());
                            }
                            catch (Exception)
                            {
                            }
                        }
                        if (!String.IsNullOrEmpty(from))
                        {
                            AddAssetAlias(from, to, atype);
                        }
                        else
                        {
                           foreach (XmlNode fnode in node.ChildNodes)
                           {
                               AddAssetAlias(fnode.InnerText, to, atype);                               
                           } 
                        }
                    }
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine("" + exception);
            }

        }

        static string GetAttribValue(XmlNode templateNode, string attribName, string defaultIfEmpty)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in templateNode.Attributes)
            {
                if (attrib.Name.ToLower() == attribName) return attrib.Value;
            }
            return defaultIfEmpty;
        }



        public static void WriteLine(string s, params object[] args)
        {
            Program.WriteLine("[ASSETS] "+s, args);
        }


        private static void AddTextureR(string p, string p_2)
        {
            AddTexture(p_2, p);
        }

        private static void AddAThuruZ(string[] strings)
        {
            char c = 'A';
            for (int i = 0; i < strings.Length; i++)
            {
                AddTextureR(strings[i], "" + c);
                c = (char)(c + 1);
            }
        }

        private static void AddTexture(string name, string uuid0, string p_3)
        {
            AddTexture(name, uuid0).Comment = p_3;
        }

        static SimAsset AddTexture(string fName, string id)
        {
            fName = fName.ToLower();
            UUID uid = UUID.Parse(id);
            SimAsset texture = FindOrCreateAsset(uid, AssetType.Texture);
            texture.NeedsRequest = false;
            texture.Name = fName;
            WorldObjects.RegisterUUID(uid, texture);
            if (Directory.Exists("texture_files/"))
            {
                byte[] bytes;
                string usedName;
                if (BytesFromFile(fName, out bytes, out usedName))
                {
                    texture.AssetData = bytes;
                    texture.Name = usedName;
                }
                else
                {
                    WriteLine("Texture w/o BVH " + fName + " " + uid);
                }
            }
            InternAsset(texture);
            return texture;
        }


        static void AnimUUID(string p, string p_2, string p_3, string p_4, string p_5)
        {
            AnimUUID(p, p_2).Comment = p_3+ " "+p_4+ " " + p_5;
        }

        static void AnimUUID(string p, string p_2, string p_3)
        {
            AnimUUID(p, p_3).Comment = p_2;
        }


        static SimAsset AnimUUID(string fName, string id)
        {
            fName = fName.ToLower();
            UUID uid = UUID.Parse(id);
            SimAsset anim = FindOrCreateAsset(uid, AssetType.Animation);
            anim.NeedsRequest = false;
            anim.Name = fName;
            WorldObjects.RegisterUUID(uid, anim);
            if (Directory.Exists("bvh_files/"))
            {
                byte[] bytes;
                string usedName;
                if (BytesFromFile(fName, out bytes, out usedName))
                {
                    anim.AssetData = bytes;
                    anim.Name = usedName;
                }
                else
                {
                   // WriteLine("Anim w/o BVH " + fName + " " + uid);
                }
            }
            InternAsset(anim);
            return anim;
        }

        public static bool BytesFromFile(string fName, out byte[] bytes, out string usedName)
        {
            usedName = fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            usedName = "avatar_" + fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            usedName = "avatar_express_" + fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            if (nameNameMap.ContainsKey(fName))
            {
                usedName = nameNameMap[fName];
                if (File.Exists("bvh_files/" + usedName + ".animation"))
                {
                    bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                    return true;
                }
            }
            bytes = null;
            return false;
        }

        static void AddAssetAlias(string from, string to)
        {
            AddAssetAlias(from, to, AssetType.Unknown);
        }

        static void AddAssetAlias(string name, string file, AssetType type)
        {
            UUID prev = TheStore.GetAssetUUID(name, type);
            if (prev != UUID.Zero)
            {
                SimAsset A = FindAsset(prev);
                if (A != null) A.Name = file;
            }
            else
            {
                prev = TheStore.GetAssetUUID(file, AssetType.Unknown);
                if (prev != UUID.Zero)
                {
                    SimAsset A = FindAsset(prev);
                    if (A != null) A.Name = name;
                }
            }
            nameNameMap[name.ToLower()] = file.ToLower();
        }

        public ICollection<string> GetAssetNames(AssetType types)
        {
            FillAssetNames();
            List<String> names = new List<String>();
            //lock (SimAssets) 
                foreach (var list in SimAssets)
                {
                    if (types==AssetType.Unknown || list.AssetType==types) names.Add(list.Name);
                }
            return names;
        }

        public String GetAssetName(UUID uuid)
        {
            FillAssetNames();
            String name;
            object anim;
            if (uuidAsset.TryGetValue(uuid, out anim))
            {
                if (anim is SimAsset)
                {
                    return ((SimAsset)anim).Name;                    
                }
            }
            return null;
        }


        public UUID GetAssetUUID(string a, AssetType type)
        {
            a = a.ToLower();
            FillAssetNames();
            UUID partial;// = default(UUID);
            if (UUID.TryParse(a,out partial))
            {
                return partial;
            }
            foreach (String name in nameAsset.Keys)
            {
                String sname = name.ToLower();
                SimAsset aset = nameAsset[name];
                if (sname.Equals(a))
                {
                    partial = aset.AssetID;
                    if (type == aset.AssetType) return partial;
                }
                if (sname.Contains(a))
                {
                    if (partial==UUID.Zero) partial = aset.AssetID;
                }
            }
            return partial;
        }

        static public SimAsset SetAssetName(UUID uUID, string s, AssetType type)
        {
            FillAssetNames();
            SimAsset anim = FindOrCreateAsset(uUID, type);
            anim.Name = s;
            if (downloadedAssetFoldersComplete)
            {
                WriteLine("SetAssetName: {0} {1}", type, s);
            }
            InternAsset(anim);
            return anim;
        }

        public static SimAsset FindAsset(UUID uUID)
        {
            object anim;
            //if (uuidAsset.TryGetValue(uUID, out anim)) return (SimAsset)anim;
            //lock (uuidAsset)
            {
                FillAssetNames();
                if (!uuidAsset.TryGetValue(uUID, out anim))
                {
                    if (false)lock (SimAssets)
                        foreach (var A in SimAssets)
                        {
                            if (A.AssetIDs.Contains(uUID))
                            {
                                return A;
                            }
                        }
                    return null;
                }
                if (!(anim is SimAsset))
                {
                    return null;
                }
                return (SimAsset)anim;
            }
        }

        public static List<UUID> Matches(String name)
        {
            List<UUID> matches = new List<UUID>();
            //lock (SimAssets) 
                foreach (var A in SimAssets)
                {
                    if (A.Matches(name))
                    {
                        matches.Add(A.AssetID);
                    }
                }
            return matches;
        }


        public static SimAsset GetSimAsset(Asset asset)
        {
            SimAsset A = FindOrCreateAsset(asset.AssetID, asset.AssetType);
            A.ServerAsset = asset;
            InternAsset(A);
            return A;
        }



        readonly static Dictionary<string, List<UUID>> meaningToAssetIDs = new Dictionary<string, List<UUID>>();

        public static List<UUID> MeaningUUIDs(string name)
        {
            lock (meaningToAssetIDs)
            {
                if (meaningToAssetIDs.ContainsKey(name))
                {
                    return meaningToAssetIDs[name];
                }
                return meaningToAssetIDs[name] = new List<UUID>();
            }
        }


        static public bool IsSitAnim(ICollection<UUID> anims)
        {
            return Matches(anims, "sit").Count != 0;
        }

        public static List<UUID> Matches(ICollection<UUID> anims, String name)
        {
            List<UUID> matches = new List<UUID>();
            foreach (UUID uuid in anims)
            {
                SimAsset A = FindAsset(uuid);
                if (A == null) continue;
                if (A.Matches(name))
                {
                    matches.Add(uuid);
                }
            }
            return matches;
        }

        public static SimAsset FindOrCreateAsset(UUID uUID, AssetType type)
        {
            SimAsset anim = FindAsset(uUID);  
            if (anim!=null)
            {
                return anim;
            }
            lock (uuidAsset)
            {
                anim = FindAsset(uUID);
                if (anim == null)
                {
                    switch (type)
                    {
                        case AssetType.Animation:
                            anim = new SimAnimation(uUID, null);
                            break;
                        case AssetType.Sound:
                        case AssetType.SoundWAV:
                            anim = new SimSound(uUID, null, type);
                            break;
                        case AssetType.Texture:
                        case AssetType.ImageJPEG:
                        case AssetType.ImageTGA:
                        case AssetType.TextureTGA:
                            anim = new SimTexture(uUID, null, type);
                            break;
                        case AssetType.Gesture:
                            anim = new SimGesture(uUID, null);
                            break;
                        case AssetType.LSLText:
                        case AssetType.LSLBytecode:
                            anim = new SimScript(uUID, null, type);
                            break;
                        case AssetType.Notecard:
                            anim = new SimNotecard(uUID, null);
                            break;
                        case AssetType.CallingCard:
                            anim = new SimCallingCard(uUID, null);
                            break;
                        case AssetType.Clothing:
                            anim = new SimClothing(uUID, null);
                            break;
                        case AssetType.Bodypart:
                            anim = new SimBodypart(uUID, null);
                            break;
                        case AssetType.Landmark:
                            anim = new SimLandmark(uUID, null);
                            break;
                        case AssetType.Link:
                            anim = new SimLandmark(uUID, null);
                            break;
                        default:
                            throw new NotImplementedException("FindOrCreateAsset " + type);
                            break;
                    }
                    uuidAsset[uUID] = anim;
                }
                anim.AssetType = type;
            }
            {
                WorldObjects.EnqueueRequestAsset(uUID, type, true);
                InternAsset(anim);
                return anim;
            }
        }

        internal readonly static ListAsSet<SimAsset> SimAssets = new ListAsSet<SimAsset>();
        internal readonly static Dictionary<string, string> nameNameMap = new Dictionary<string, string>();
        static private bool FilledInAssets;

        public static ICollection<SimAsset> GetAssets(AssetType assetType)
        {
            return SimAssets.CopyOf();
        }

        public static bool SameAsset(SimAsset animation, SimAsset simAnimation)
        {
            if (animation == null || simAnimation == null) return false;
            return (animation.SameAsset(simAnimation));
        }

        public void Dispose()
        {
           taskQueue.Dispose();
        }

        public SimAsset GetAnimationOrGesture(string animate)
        {
            if (animate.StartsWith("anim:")) animate = animate.Substring(5);
            UUID uuid = GetAssetUUID(animate, AssetType.Animation);
            if (uuid == UUID.Zero) uuid =  GetAssetUUID(animate+".csv", AssetType.Animation);
            if (uuid != UUID.Zero) return FindAsset(uuid);
            uuid = GetAssetUUID(animate, AssetType.Gesture);
            if (uuid != UUID.Zero) return FindAsset(uuid);
            return null;
        }

        public void Enqueue(Action action)
        {
            taskQueue.Enqueue(new ThreadStart(action));
        }
    }

#if PORTIT

    /// <summary>
    /// Asset request download handler, allows a configurable number of download slots
    /// </summary>
    public class AssetPipeline
    {
        class TaskInfo
        {
            public UUID RequestID;
            public int RequestNbr;
            public AssetType Type;

            public TaskInfo(UUID reqID, int reqNbr, AssetType type)
            {
                RequestID = reqID;
                RequestNbr = reqNbr;
                Type = type;
            }
        }

        public delegate void DownloadFinishedCallback(UUID id, bool success);
        public delegate void DownloadProgressCallback(UUID image, int recieved, int total);

        /// <summary>Fired when a texture download completes</summary>
        public event DownloadFinishedCallback OnDownloadFinished;
        /// <summary>Fired when some texture TypeData is received</summary>
        public event DownloadProgressCallback OnDownloadProgress;

        public int CurrentCount { get { return currentRequests.Count; } }
        public int QueuedCount { get { return requestQueue.Count; } }

        GridClient client;
        /// <summary>Maximum concurrent texture requests</summary>
        int maxAssetRequests;
        /// <summary>Queue for image requests that have not been sent out yet</summary>
        List<TaskInfo> requestQueue;
        /// <summary>Current texture downloads</summary>
        Dictionary<UUID, int> currentRequests;
        /// <summary>Storage for completed texture downloads</summary>
        Dictionary<UUID, ImageDownload> completedDownloads;
        AutoResetEvent[] resetEvents;
        int[] threadpoolSlots;
        Thread downloadMaster;
        bool running;
        object syncObject = new object();

        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="client">Reference to <code>SecondLife</code> client</param>
        /// <param name="maxRequests">Maximum number of concurrent texture requests</param>
        public AssetPipeline(GridClient client, int maxRequests)
        {
            running = true;
            this.client = client;
            maxAssetRequests = maxRequests;

            requestQueue = new List<TaskInfo>();
            currentRequests = new Dictionary<UUID, int>(maxAssetRequests);
            completedDownloads = new Dictionary<UUID, ImageDownload>();
            resetEvents = new AutoResetEvent[maxAssetRequests];
            threadpoolSlots = new int[maxAssetRequests];

            // Pre-configure autoreset events/download slots
            for (int i = 0; i < maxAssetRequests; i++)
            {
                resetEvents[i] = new AutoResetEvent(false);
                threadpoolSlots[i] = -1;
            }

            client.Assets.OnImageReceived += Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress += Assets_OnImageReceiveProgress;

            // Fire up the texture download thread
            downloadMaster = new Thread(new ThreadStart(DownloadThread));
            downloadMaster.Start();
        }

        public void Shutdown()
        {
            client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress -= Assets_OnImageReceiveProgress;

            requestQueue.Clear();

            for (int i = 0; i < resetEvents.Length; i++)
                if (resetEvents[i] != null)
                    resetEvents[i].Set();

            running = false;
        }

        /// <summary>
        /// Request a texture be downloaded, once downloaded OnImageRenderReady event will be fired
        /// containing texture key which can be used to retrieve texture with GetAssetToRender method
        /// </summary>
        /// <param name="textureID">Asset to request</param>
        /// <param name="type">Type of the requested texture</param>
        public void RequestAsset(UUID textureID, AssetType type0)
        {
            lock (syncObject)
            {
                if (client.Assets.Cache.HasImage(textureID))
                {
                    // Add to rendering dictionary
                    if (!completedDownloads.ContainsKey(textureID))
                    {
                        completedDownloads.Add(textureID, client.Assets.Cache.GetCachedImage(textureID));

                        // Let any subscribers know about it
                        if (OnDownloadFinished != null)
                            OnDownloadFinished(textureID, true);
                    }
                    else
                    {
                        // This image has already been served up, ignore this request
                    }
                }
                else
                {
                    // Make sure the request isn't already queued up
                    foreach (TaskInfo task in requestQueue)
                    {
                        if (task.RequestID == textureID)
                            return;
                    }

                    // Make sure we aren't already downloading the texture
                    if (!currentRequests.ContainsKey(textureID))
                        requestQueue.Add(new TaskInfo(textureID, 0, type0));
                }
            }
        }

        /// <summary>
        /// retrieve texture information from dictionary
        /// </summary>
        /// <param name="textureID">Asset ID</param>
        /// <returns>ImageDownload object</returns>
        public ImageDownload GetAssetToRender(UUID textureID)
        {
            lock (syncObject)
            {
                if (completedDownloads.ContainsKey(textureID))
                {
                    return completedDownloads[textureID];
                }
                else
                {
                    Logger.Log("Requested texture data for texture that does not exist in dictionary", Helpers.LogLevel.Warning);
                    return null;
                }
            }
        }

        /// <summary>
        /// Remove no longer necessary texture from dictionary
        /// </summary>
        /// <param name="textureID"></param>
        public bool RemoveFromPipeline(UUID textureID)
        {
            lock (syncObject)
                return completedDownloads.Remove(textureID);
        }

        public void AbortDownload(UUID textureID)
        {
            lock (syncObject)
            {
                for (int i = 0; i < requestQueue.Count; i++)
                {
                    TaskInfo task = requestQueue[i];

                    if (task.RequestID == textureID)
                    {
                        requestQueue.RemoveAt(i);
                        --i;
                    }
                }

                int current;
                if (currentRequests.TryGetValue(textureID, out current))
                {
                    currentRequests.Remove(textureID);
                    resetEvents[current].Set();

                    // FIXME: Send an abort packet
                }
            }
        }

        /// <summary>
        /// Master Download Thread, Queues up downloads in the threadpool
        /// </summary>
        private void DownloadThread()
        {
            int reqNbr;

            while (running)
            {
                if (requestQueue.Count > 0)
                {
                    reqNbr = -1;
                    // find available slot for reset event
                    for (int i = 0; i < threadpoolSlots.Length; i++)
                    {
                        if (threadpoolSlots[i] == -1)
                        {
                            threadpoolSlots[i] = 1;
                            reqNbr = i;
                            break;
                        }
                    }

                    if (reqNbr != -1)
                    {
                        TaskInfo task = null;
                        lock (syncObject)
                        {
                            if (requestQueue.Count > 0)
                            {
                                task = requestQueue[0];
                                requestQueue.RemoveAt(0);
                            }
                        }

                        if (task != null)
                        {
                            task.RequestNbr = reqNbr;

                            Logger.DebugLog(String.Format("Sending Worker thread new download request {0}", reqNbr));
                            ThreadPool.QueueUserWorkItem(AssetRequestDoWork, task);
                            continue;
                        }
                    }
                }

                // Queue was empty, let's give up some CPU time
                Thread.Sleep(500);
            }

            Logger.Log("Asset pipeline shutting down", Helpers.LogLevel.Info);
        }

        private void AssetRequestDoWork(Object threadContext)
        {
            TaskInfo ti = (TaskInfo)threadContext;

            lock (syncObject)
            {
                if (currentRequests.ContainsKey(ti.RequestID))
                {
                    threadpoolSlots[ti.RequestNbr] = -1;
                    return;
                }
                else
                {
                    currentRequests.Add(ti.RequestID, ti.RequestNbr);
                }
            }

            Logger.DebugLog(String.Format("Worker {0} Requesting {1}", ti.RequestNbr, ti.RequestID));

            resetEvents[ti.RequestNbr].Reset();
            client.Assets.RequestAsset(ti.RequestID, ti.Type,true);

            // don't release this worker slot until texture is downloaded or timeout occurs
            if (!resetEvents[ti.RequestNbr].WaitOne(45 * 1000, false))
            {
                // Timed out
                Logger.Log("Worker " + ti.RequestNbr + " Timeout waiting for Asset " + ti.RequestID + " to Download", Helpers.LogLevel.Warning);

                lock (syncObject)
                    currentRequests.Remove(ti.RequestID);

                if (OnDownloadFinished != null)
                    OnDownloadFinished(ti.RequestID, false);
            }

            // free up this download slot
            threadpoolSlots[ti.RequestNbr] = -1;
        }

        private void Assets_OnImageReceived(ImageDownload image, Asset asset)
        {
            int requestNbr;
            bool found;

            lock (syncObject)
                found = currentRequests.TryGetValue(image.ID, out requestNbr);

            if (asset != null && found)
            {
                Logger.DebugLog(String.Format("Worker {0} Downloaded texture {1}", requestNbr, image.ID));

                // Free up this slot in the ThreadPool
                lock (syncObject)
                    currentRequests.Remove(image.ID);

                resetEvents[requestNbr].Set();

                if (image.Success)
                {
                    // Add to the completed texture dictionary
                    lock (syncObject)
                        completedDownloads[image.ID] = image;
                }
                else
                {
                   // Logger.Log(String.Format("Download of texture {0} failed. NotFound={1}", image.ID, image.NotFound),
                     //   Helpers.LogLevel.Warning);
                }

                // Let any subscribers know about it
                if (OnDownloadFinished != null)
                    OnDownloadFinished(image.ID, image.Success);
            }
        }

        private void Assets_OnImageReceiveProgress(UUID image, int lastPacket, int recieved, int total)
        {
            if (OnDownloadProgress != null && currentRequests.ContainsKey(image))
                OnDownloadProgress(image, recieved, total);
        }
    }
#endif
}