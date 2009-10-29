using System;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class CogbotContextMenuListener
    {
        private CogbotContextMenuListener _cogbotContextMenu;
        private RadegastContextMenuStrip TheirInterest;
        public CogbotContextMenuListener()
        {
            _cogbotContextMenu = this;
            RadegastContextMenuStrip.OnContentMenuOpened += Test_OnContentMenuOpened;
            RadegastContextMenuStrip.OnContentMenuItemSelected += Test_OnContentMenuItemSelected;
            RadegastContextMenuStrip.OnContentMenuItemClicked += Test_OnContentMenuItemClicked;
            RadegastContextMenuStrip.OnContentMenuClosing += Test_OnContentMenuClosing;
        }
    
        private void Test_OnContentMenuItemClicked(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            if (e.MenuItem==null) return;
            WriteLine("I hope you meant to " + e.MenuItem.Text + "  " + e.Selection + "!");
            if (!e.MenuItem.Enabled)
            {
                WriteLine("If not do not worry it was not enabled ");
            }
        }

        static void WriteLine(string s)
        {
           //Console.WriteLine(s);
        }

        private void Test_OnContentMenuItemSelected(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                if (e.MenuItem == null)
                {
                    WriteLine("The last menu selection is not hightlighted by the mouse anymore so do not click");
                }
                else if (!e.MenuItem.Enabled)
                {
                    WriteLine("You cannot " + e.MenuItem.Text + " at this time to " + e.Selection);
                }
                else
                {
                    WriteLine("You can " + e.MenuItem.Text + " " + e.Selection + " if you press enter or click");
                }
            }
        }
    
        private void Test_OnContentMenuClosing(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                WriteLine("You can no longer see the Menu: " + TheirInterest);
                TheirInterest = null;
            }
        }
    
        private void Test_OnContentMenuOpened(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                TheirInterest = e.Menu;
                WriteLine("You are looking at Menu: " + TheirInterest);
                WriteLine("The Item you are going to do something to is: " + e.Selection);
                foreach (var item in e.Menu.AllChoices())
                    if (item.Enabled)
                        WriteLine(" You can: " + item.Text);
                    else
                        WriteLine(" cannot: " + item.Text);
            }
        }
    }
}