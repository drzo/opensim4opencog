using System;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Land
{
    public class GridLayerCommand : Command, GridMasterCommand
    {
        private bool registeredCallback = false;

        public GridLayerCommand(BotClient testClient)
        {
            Name = "gridlayer";
        }

        public override void MakeInfo()
        {
            Description = "Downloads all of the layer chunks for the grid object map";
            Category = CommandCategory.Simulator;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (!registeredCallback)
            {
                registeredCallback = true;
                Client.Grid.GridLayer += Grid_OnGridLayer;
            }
            Client.Grid.RequestMapLayer(GridLayerType.Objects);

            return Success("Sent " + Name);
        }

        private void Grid_OnGridLayer(object sender, GridLayerEventArgs e)
        {
            GridLayer layer = e.Layer;
            WriteLine(String.Format("Layer({0}) Bottom: {1} Left: {2} Top: {3} Right: {4}",
                                    layer.ImageID.ToString(), layer.Bottom, layer.Left, layer.Top, layer.Right));
            //string filename = layer.
            //Client.ExecuteCommand("download " + layer.ImageID.ToString() + " " + AssetType.ImageTGA + " " + filename);
        }
    }
}