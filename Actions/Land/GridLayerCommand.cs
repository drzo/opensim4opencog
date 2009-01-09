using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class GridLayerCommand : Command
    {
        public GridLayerCommand(cogbot.TextForm testClient)
        {
            Name = "gridlayer";
            Description = "Downloads all of the layer chunks for the grid object map";
            Category = CommandCategory.Simulator;

             testClient.client.Grid.OnGridLayer += new GridManager.GridLayerCallback(Grid_OnGridLayer);
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            Client.Grid.RequestMapLayer(GridLayerType.Objects);

            return "Sent.";
        }

        private void Grid_OnGridLayer(GridLayer layer)
        {
            WriteLine(String.Format("Layer({0}) Bottom: {1} Left: {2} Top: {3} Right: {4}",
                layer.ImageID.ToString(), layer.Bottom, layer.Left, layer.Top, layer.Right));
        }
    }
}
