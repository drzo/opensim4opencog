using System;
using System.Collections.Generic;
using System.IO;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Http;
using System.Threading;
using System.Windows.Forms;
using Logger = OpenMetaverse.Logger;

namespace cogbot.Actions
{
#if PORTIT
    public class CreateScriptCommand : Command
    {
        Exception problem;
        public CreateScriptCommand(BotClient testClient)
        {
            Name = "createscript";
            Description = "Creates a script from a local text file.";
            Category = CommandCategory.Inventory;
        }

        public void RequestCreateItemFromAsset(byte[] data, string name, string description, AssetType assetType,
           InventoryType invType, UUID folderID,ManualResetEvent done)
        {
            problem = null;
            Client.Inventory.RequestCreateItemFromAsset(data, name, description, assetType, invType, folderID, delegate(CapsClient client, long bytesReceived, long bytesSent,
            long totalBytesToReceive, long totalBytesToSend)
            {
                Console.WriteLine("Progress " + name + " {0}/{1} {2}/{3}", bytesReceived, totalBytesToReceive, bytesSent, totalBytesToSend);
            }, delegate(bool success, string status, UUID itemID, UUID assetID)
            {
                string str = "Create " + (success ? "successfull " : "failed") + " for " + name + "  as " + assetID + " status " + status;
                Console.WriteLine(str);
                if (!success) problem = new Exception(str);
                done.Set();
            });
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
                return "Usage: createscript filename.txt";

            string file = String.Empty;
            for (int ct = 0; ct < args.Length; ct++)
                file = file + args[ct] + " ";
            file = file.TrimEnd();

            WriteLine("Filename: {0}", file);
            if (!File.Exists(file))
                return String.Format("Filename '{0}' does not exist", file);

            System.IO.StreamReader reader = new StreamReader(file);
            string body = reader.ReadToEnd();

            try
            {
                MakeScript(file, body);
                return "Done";
            }
            catch (System.Exception e)
            {
                return "Error creating script. " + e;
            }
        }

        public void MakeScript(string file, string body)
        {
            try
            {
                if (Client.Network.CurrentSim==null)
                Client.Network.CurrentSim = Client.Network.Simulators[0];
                Client.Settings.LOG_ALL_CAPS_ERRORS = true;
                ManualResetEvent done = new ManualResetEvent(false);
                new Thread(new ThreadStart(delegate()
                {
                    MakeScript1(file, body, done);
                })).Start();

                bool complete = true;
                while (!complete) {
                    complete = done.WaitOne(50,false);
                    Application.DoEvents();
                    Thread.Sleep(500);
                }
            }
            catch (System.Exception e)
            {
                Logger.Log(e.ToString(), Helpers.LogLevel.Error, Client);
                throw e;
            }
            if (problem != null) throw problem;
        }

        private void MakeScript1(string file, string body, ManualResetEvent done)
        {
            string desc = String.Format("{0} created by Cogbot.exe {1}", file, DateTime.Now);
            // create the asset
            byte[] data = CreateScriptAsset(body);
            RequestCreateItemFromAsset(data, file, desc,
                AssetType.LSLText, InventoryType.LSL,
                Client.Inventory.FindFolderForType(AssetType.LSLText), done);
        }
        
        /// <summary>
        /// </summary>
        /// <param name="body"></param>
        public static byte[] CreateScriptAsset(string body)
        {
           AssetScriptText ast = new AssetScriptText(body);
           //UUID assetID = UUID.Random();
           ast.Encode();
          // ast = new AssetScriptText(assetID, ast.AssetData);
           return  ast.AssetData;
        }
    }
#endif
}