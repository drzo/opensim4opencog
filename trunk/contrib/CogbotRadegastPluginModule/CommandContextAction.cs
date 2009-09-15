using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using OpenMetaverse;
using Radegast;
using PathSystem3D.Navigation;
using Action=System.Action;

namespace CogbotRadegastPluginModule
{
    public class CommandContextAction : ContextAction
    {
        public Object lastObject;
        public cogbot.Actions.Action act;
        public Type useType;
        CogbotTabWindow console
        {
            get { return (CogbotTabWindow)instance.TabConsole.GetTab("cogbot").Control; }
        }
        private ContextMenuStrip ExtraContextMenu
        {
            get { return console.PluginExtraContextMenu; }
        }
        public CommandContextAction(RadegastInstance radegastInstance)
            : base(radegastInstance)
        {
            ContextType = typeof (Object);
            Label = "commands...";
            Client.Network.OnLogin += aspectLogin;
        }

        public Dictionary<string, List<ToolStripMenuItem>> MenuItems = new Dictionary<string, List<ToolStripMenuItem>>();
        private void aspectLogin(LoginStatus login, string message)
        {
            if (login!=LoginStatus.Success) return;
            ScanCogbotMenu();
        }

        private void ScanCogbotMenu()
        {
            if (act!=null) return;
            foreach (var c in ClientManager.SingleInstance.groupActions.Values)
            {
                AddCommand(c);
            }
            foreach (var c in ClientManager.SingleInstance.LastBotClient.Commands.Values)
            {
                AddCommand(c);
            }
        }

        private void AddCommand(cogbot.Actions.Action c)
        {
            if (string.IsNullOrEmpty(c.Name))
            {
                throw new AbandonedMutexException();
            }
            if (c.Name.StartsWith("!")) return;
            if (c.Parameters!=null)
            {
                int i = 0;
                while (i<c.Parameters.Length)
                {
                    Type from = c.Parameters[i];
                    i++;
                    Type use = c.Parameters[i];
                    AddCommand(c,from,use);

                    i++;
                }
            }
        }

        static public readonly HashSet<cogbot.Actions.Action> Actions = new HashSet<cogbot.Actions.Action>();
        private void AddCommand(cogbot.Actions.Action renCmd, Type type, Type use)
        {
            lock (Actions) if (!Actions.Add(renCmd)) return;
            CommandContextAction cca = new CommandContextAction(instance)
                                           {
                                               Label = renCmd.Name,
                                               Handler = SubHook,
                                               ContextType = type,
                                               act = renCmd,
                                               useType = use
                };
            instance.TabConsole.AddContextMenu(cca);

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
            object obj = GetValue(useType);
            DebugLog(ActName + "=" + obj);

            TryCatch(() =>
                         {
                             instance.CommandsManager.ExecuteCommand("//thread " + ActName + " " + obj);
                         });

        }

        public string ActName
        {
            get { return act.Name.Replace(" ", "").ToLower(); }
        }

        public override bool Contributes(object o, Type type)
        {
            //if (type == useType) return true;           
            if (base.ContextType == typeof(SimPosition))
            {
                if (type == typeof(UUID)) return false;
                return !typeof(InventoryBase).IsInstanceOfType(o);
            }
            return base.Contributes(o, type);
        }

        public override bool IsEnabled(object target)
        {
            return true;
        }

        public override IEnumerable<ToolStripMenuItem> GetToolItems(object target, Type type)
        {
            if (act == null) return null;
            List<ToolStripMenuItem> lst = new List<ToolStripMenuItem>();
            lastObject = target;
            {
                lst.Add(new ToolStripMenuItem(act.Name, null, SubHook)
                            {
                                ToolTipText = act.makeHelpString()
                            });

            }
            return lst;
        }

        public object GetValue(Type type)
        {
            object o = GetValue(type, lastObject);
            if (type == null) return null;
            if (type.IsInstanceOfType(o)) return o;
            object od = DeRef(o);
            if (od != o) return GetValue(type, od);
            return o;
        }

        public object GetValue(Type type, object lastObject)
        {
            if (type == null) return lastObject;
            if (type.IsInstanceOfType(lastObject)) return lastObject;
            if (type.IsAssignableFrom(typeof(Vector2))) return ToUUID(lastObject).ToString();                        
            if (type.IsAssignableFrom(typeof(SimPosition))) return ToUUID(lastObject).ToString();
            if (type.IsAssignableFrom(typeof(string))) return ToUUID(lastObject).ToString();
            if (type.IsAssignableFrom(typeof(Primitive))) return ToPrimitive(lastObject);
            if (type.IsAssignableFrom(typeof(Avatar))) return ToAvatar(lastObject);
            if (type.IsAssignableFrom(typeof(UUID))) return ToUUID(lastObject);
            return lastObject;
        }
        public override void Dispose()
        {
            if (act == null) lock (Actions) Actions.Clear();
        }
    }
}