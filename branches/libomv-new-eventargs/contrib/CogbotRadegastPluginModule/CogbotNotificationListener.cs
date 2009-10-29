using System;
using System.Collections.Generic;
using System.Windows.Forms;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class CogbotNotificationListener
    {
        private CogbotNotificationListener _cogbotNotification;

        private List<NotificationEventArgs> LookingAt = new List<NotificationEventArgs>();

        public CogbotNotificationListener()
        {
            _cogbotNotification = this;
            Notification.OnNotificationDisplayed += Test_OnNotificationOpened;
        }

        private void Test_OnNotificationItemClicked(object sender, EventArgs e, NotificationEventArgs notice)
        {
            lock (_cogbotNotification)
            {
                notice.OnNotificationClicked -= Test_OnNotificationItemClicked;
                lock (LookingAt) if (!LookingAt.Contains(notice))
                    {
                        WriteLine("Not sure you heard that we tracking " + notice.Text);
                    }
                Button button = sender as Button;
                if (button == null)
                {
                    WriteLine("You expected a button but got: " + sender + " for " + notice.Text);
                    return;
                }
                WriteLine("You clicked: " + button.Text + " on Dialog" + notice.Text);
            }
        }

        static void WriteLine(string s)
        {
           // Console.WriteLine(s);
        }

        private void Test_OnNotificationClosing(object sender, NotificationEventArgs e)
        {
            lock (_cogbotNotification)
            {
                WriteLine("You can no longer see the Notifation: " + e.Text);
                e.OnNotificationClosed -= Test_OnNotificationClosing;
                lock (LookingAt) LookingAt.Remove(e);
            }
        }

        private void Test_OnNotificationOpened(object sender, NotificationEventArgs e)
        {
            lock (_cogbotNotification)
            {
                lock (LookingAt) LookingAt.Add(e);
                WriteLine("Hooked up " + e.Text);
                // Hook me up
                e.OnNotificationClicked += Test_OnNotificationItemClicked;
                e.OnNotificationClosed += Test_OnNotificationClosing;
            }
        }
    }
}