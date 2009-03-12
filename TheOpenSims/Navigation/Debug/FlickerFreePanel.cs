using System;
using System.Windows.Forms;


namespace cogbot.TheOpenSims.Navigation.Debug
{
	/// <summary>
	/// Summary description for FlickerFreePanel.
	/// </summary>
    public partial class FlickerFreePanel : Panel
	{
		public FlickerFreePanel()
		{
			SetStyle(ControlStyles.ResizeRedraw, true);
			SetStyle(ControlStyles.AllPaintingInWmPaint, true);
			SetStyle(ControlStyles.DoubleBuffer, true);
			SetStyle(ControlStyles.UserPaint, true);
		}
	}
}
