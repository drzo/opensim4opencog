using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows.Forms;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class SimUsageContextAction : ContextAction
    {
        public Object lastObject;
        public readonly CogbotRadegastPlugin Plugin;
        CogbotTabWindow console
        {
            get { return (CogbotTabWindow)instance.TabConsole.GetTab("cogbot").Control; }
        }
        private RadegastContextMenuStrip ExtraContextMenu
        {
            get { return console.PluginExtraContextMenu; }
        }
        public SimUsageContextAction(RadegastInstance radegastInstance, CogbotRadegastPlugin plugin)
            : base(radegastInstance)
        {
            ContextType = typeof(Object);
            Label = "SimUsageContextAction...";
            Plugin = plugin;
        }


        private void HookItem(ToolStripDropDownItem t)
        {
            t.Click += SubHook;
            t.Tag = this;
            if (!t.HasDropDownItems) return;
            foreach (ToolStripMenuItem item in t.DropDownItems)
            {
                HookItem(item);
            }
        }

        private void SubHook(object sender, EventArgs e)
        {
            TryCatch(() =>
                         {
                             if (sender != lastObject && sender is ToolStripItem)
                                 FakeEvent(sender, "Click", lastObject, e);
                         });
            instance.TabConsole.DisplayNotificationInChat(
                string.Format("SubHook sender={0}\nlastObect={1}", ToString(sender),
                              ToString(lastObject)));
        }

        public void FakeEvent(Object target, String infoName, params object[] parameters)
        {
            Type type = target.GetType();
            EventInfo eventInfo = type.GetEvent(infoName);
            MethodInfo m = eventInfo.GetRaiseMethod();

            Exception lastException = null;
            if (m != null)
            {
                try
                {


                    m.Invoke(target, parameters);
                    return;
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }
            else
            {
                {
                    FieldInfo fieldInfo = type.GetField(eventInfo.Name,
                                                        BindingFlags.Instance | BindingFlags.NonPublic |
                                                        BindingFlags.Public);
                    if (fieldInfo != null)
                    {
                        Delegate del = fieldInfo.GetValue(target) as Delegate;

                        if (del != null)
                        {
                            del.DynamicInvoke(parameters);
                            return;
                        }
                    }
                }
            }
            if (lastException != null) throw lastException;
            throw new NotSupportedException();
        }

        public override bool Contributes(object o, Type type)
        {
            return true;
        }
        public override bool IsEnabled(object target)
        {
            return true;
        }
        public override IEnumerable<ToolStripMenuItem> GetToolItems(object target, Type type)
        {
            List<ToolStripMenuItem> lst = new List<ToolStripMenuItem>();
            SimObject O = GetSimObject(target);
            if (O == null) return lst;
            foreach (var cl in O.GetTypeUsages())
            {
                var c = cl;
                string name = "Do " + c;
                lst.Add(new ToolStripMenuItem(name, null, (sender, e) => InvokeThis(c, sender, e, O, target, type))
                            {
                                ToolTipText = name + " oo " + target
                            });

            }
            return ListOfOne("Do..", lst);

        }

        public SimObject GetSimObject(object target)
        {
            SimObject O = null;
            UUID id = ToUUID(target);
            if (id != UUID.Zero)
                O = WorldObjects.GetSimObjectFromUUID(id);
            else
            {
                if (target is SimObject)
                {
                    O = (target as SimObject);
                }
            }
            return O;
        }

        private IEnumerable<ToolStripMenuItem> ListOfOne(string name, List<ToolStripMenuItem> items)
        {
            List<ToolStripMenuItem> lst = new List<ToolStripMenuItem>();
            lst.Add(new ToolStripMenuItem(name, null, items.ToArray())
            {
            });
            return lst;
        }

        private void InvokeThis(SimTypeUsage name, object sender, EventArgs args, SimObject O, object target, Type type)
        {

            Plugin.TheBot.WorldSystem.TheSimAvatar.Do(name, O ?? GetSimObject(target));
        }

        //private void AddCallback(object target, ToolStripMenuItem item)
        //{
        //    if (item.HasDropDownItems)
        //    {
        //        foreach (var pair in item.DropDownItems)
        //        {
        //            AddCallback(target, item); 
        //        }
        //    }
        //    EventHandler act = (sender, e) =>
        //           instance.TabConsole.DisplayNotificationInChat(
        //               string.Format(" sender={0}\ntarget={1}", ToString(sender), ToString(target)));
        //    item.Click += act;

        //    EventHandler reg = (sender, e) =>
        //                             {

        //                             };
        //    EventHandler ureg = (sender, e) =>
        //    {

        //    };
        //    //item.OwnerChanged += reg;


        //    EventHandler dereg = (sender, e) =>
        //                             {
        //                                 item.Click -= act;
        //                                 item.Click += act;
        //                             };

        //    item.OwnerChanged += dereg;
        //    item.LocationChanged += dereg;
        //}

        private string ToString(object sender)
        {
            string t = sender.GetType().Name + ":";
            if (sender is Control)
            {
                Control control = (Control)sender;
                return string.Format("{0}{1} {2} {3}", t, control.Text, control.Name, ToString(control.Tag));
            }
            if (sender is ListViewItem)
            {
                ListViewItem control = (ListViewItem)sender;
                return string.Format("{0}{1} {2} {3}", t, control.Text, control.Name, ToString(control.Tag));
            }
            return t + sender;
        }
        //public override string LabelFor(object target)
        //{
        //    return target.GetType().Name;
        //}
        public object GetValue(Type type)
        {
            if (type.IsInstanceOfType(lastObject)) return lastObject;
            if (type.IsAssignableFrom(typeof(Primitive))) return ToPrimitive(lastObject);
            if (type.IsAssignableFrom(typeof(Avatar))) return ToAvatar(lastObject);
            if (type.IsAssignableFrom(typeof(UUID))) return ToUUID(lastObject);
            return lastObject;
        }
    }
}