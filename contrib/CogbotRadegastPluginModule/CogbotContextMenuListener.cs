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
            Console.WriteLine("I hope you meant to " + e.MenuItem.Text + "  " + e.Selection + "!");
            if (!e.MenuItem.Enabled)
            {
                Console.WriteLine("If not do not worry it was not enabled ");
            }
        }
    
        private void Test_OnContentMenuItemSelected(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                if (e.MenuItem == null)
                {
                    Console.WriteLine("The last menu selection is not hightlighted by the mouse anymore so do not click");
                }
                else if (!e.MenuItem.Enabled)
                {
                    Console.WriteLine("You cannot " + e.MenuItem.Text + " at this time to " + e.Selection);
                }
                else
                {
                    Console.WriteLine("You can " + e.MenuItem.Text + " " + e.Selection + " if you press enter or click");
                }
            }
        }
    
        private void Test_OnContentMenuClosing(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                Console.WriteLine("You can no longer see the Menu: " + TheirInterest);
                TheirInterest = null;
            }
        }
    
        private void Test_OnContentMenuOpened(object sender, RadegastContextMenuStrip.ContextMenuEventArgs e)
        {
            lock (_cogbotContextMenu)
            {
                TheirInterest = e.Menu;
                Console.WriteLine("You are looking at Menu: " + TheirInterest);
                Console.WriteLine("The Item you are going to do something to is: " + e.Selection);
                foreach (var item in e.Menu.AllChoices())
                    if (item.Enabled)
                        Console.WriteLine(" You can: " + item.Text);
                    else
                        Console.WriteLine(" cannot: " + item.Text);
            }
        }
    }
}