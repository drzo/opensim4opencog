using System;
using System.Collections.Generic;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;
using PathSystem3D.Navigation;

namespace CogbotRadegastPluginModule
{
    public class CommandContextAction : ContextAction
    {
        public Object lastObject;
        public Command act;
        public Type useType;
        public readonly CogbotRadegastPlugin Plugin;
        CogbotTabWindow console
        {
            get { return (CogbotTabWindow)instance.TabConsole.GetTab("cogbot").Control; }
        }
        private RadegastContextMenuStrip ExtraContextMenu
        {
            get { return console.PluginExtraContextMenu; }
        }
        public CommandContextAction(RadegastInstance radegastInstance, CogbotRadegastPlugin plugin)
            : base(radegastInstance)
        {
            ContextType = typeof(Object);
            Label = "commands...";
            Client.Network.LoginProgress += aspectLogin;
            Plugin = plugin;
        }

        public Dictionary<string, List<ToolStripMenuItem>> MenuItems = new Dictionary<string, List<ToolStripMenuItem>>();
        private void aspectLogin(object sender, LoginProgressEventArgs e)
        {
            if (e.Status != LoginStatus.Success) return;
            ScanCogbotMenu();
        }

        private void ScanCogbotMenu()
        {
            if (act != null) return;
            int groupCommands = 0, botCommands = 0;

            if (ClientManager.SingleInstance.groupActions != null)
            {
                lock (ClientManager.SingleInstance.groupActions)
                    foreach (var c in ClientManager.SingleInstance.groupActions.Values)
                    {
                        if (AddCommand(c)) groupCommands++;
                    }
            }
            if (TheBotClient != null && TheBotClient.Commands != null)
            {
                lock (TheBotClient.Commands)
                    foreach (var c in TheBotClient.Commands.Values)
                    {
                        if (AddCommand(c)) botCommands++;
                    }
            }
            DebugLog(string.Format("Loaded groupCommands={0} botCommands={1}", groupCommands, botCommands));
        }

        protected BotClient TheBotClient
        {
            get { return Plugin.TheBot; }
        }

        private bool AddCommand(Command c)
        {
            if (c == null)
            {
                DebugLog("WARNING: Cannot use NULL command");
                return false;
            }
            string cName = c.Name;
            if (string.IsNullOrEmpty(cName))
            {
                DebugLog("WARNING: Cannot use no-name command " + c.GetType());
                return false;
            }
            if (cName.StartsWith("!"))
            {
                //DebugLog("WARNING: Skipping command " + cName);
                return false;
            }
            if (c.Parameters == null)
            {
                // DebugLog("WARNING: Skipping non-paramerized command " + cName);
                return false;
            }
            int i = 0;
            while (i < c.Parameters.Length)
            {
                Type from = (Type)c.Parameters[i].Key;
                Type use = (Type)c.Parameters[i].Value;
                AddCommand(c, from, use);
                i++;
            }
            return true;
        }

        static public readonly HashSet<Command> Actions = new HashSet<Command>();
        private void AddCommand(Command renCmd, Type type, Type use)
        {
            lock (Actions)
            {

                if (!Actions.Add(renCmd)) return;

                CommandContextAction cca = new CommandContextAction(instance, Plugin)
                                               {
                                                   Label = renCmd.Name,
                                                   Handler = null,
                                                   ContextType = type,
                                                   act = renCmd,
                                                   useType = use
                                               };
                RegisterConextAction(renCmd,cca);
            }
        }

        static public readonly Dictionary<String, CommandContextMenu> ActionSubMenu = new Dictionary<String, CommandContextMenu>();
        private CommandContextMenu GetCommandContextMenu(String subDir)
        {
            int i = subDir.LastIndexOf(".");
            if (i > 0) subDir = subDir.Substring(i + 1);
            CommandContextMenu ccc;           
            if (ActionSubMenu.TryGetValue(subDir,out ccc)) return ccc;
            ccc = new CommandContextMenu(instance, Plugin)
            {
                Label = subDir,
                Handler = null,
                ContextType = typeof(object),
                act = null,
                useType = typeof(object)
            };
            ActionSubMenu[subDir] = ccc;
            instance.TabConsole.RegisterContextAction(ccc);
            return ccc;
        }

        private void RegisterConextAction(Command t, CommandContextAction cca)
        {            
            String subDir = t.GetType().Namespace;
            var ccc = GetCommandContextMenu(subDir);
            ccc.AddSubCommand(cca);
            var ccc2 = GetCommandContextMenu(t.Category.ToString());
            if (ccc2!=ccc) ccc2.AddSubCommand(cca);
            //instance.TabConsole.RegisterContextAction(cca);
        }

        public override void OnInvoke(object sender, EventArgs e, object target)
        {
            
            //base.OnInvoke(sender, e, target);
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
            if (obj is SimObject)
            {
                obj = ((SimObject)obj).ID.ToString();
            }
            else if (obj is Primitive)
            {
                obj = ((Primitive)obj).ID.ToString();
            }
            else if (obj is SimPosition)
            {
                obj = ((SimPosition)obj).GlobalPosition.ToString();
            }

            DebugLog(ActName + "=" + obj);

            TryCatch(() =>
                         {
                             instance.CommandsManager.ExecuteCommand("//thread " + ActName + " " + obj);
                         });

        }

        virtual public string ActName
        {
            get
            {
                string name = act.Name.Replace(" ", "").ToLower();
                while (name.EndsWith(".")) name = name.Substring(0, name.Length - 1);
                return name;
            }
        }

        public override bool Contributes(object o, Type type)
        {
            if (type == useType) return true;
            if (base.ContextType == typeof(SimPosition) || base.ContextType == typeof(SimObject) || base.ContextType == typeof(SimAvatar))
            {
                if (type == typeof(UUID))
                {
                    return false;
                }
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
            return ListOfOne("Commands...", lst);
        }

        static public IEnumerable<ToolStripMenuItem> ListOfOne(string commands, List<ToolStripMenuItem> items)
        {
            return items;
        }

        public object GetValue(Type type)
        {
            if (type == typeof(UUID) && lastObject is SimObject) return ((SimObject)lastObject).ID;
            object o = GetValue(type, lastObject);
            if (type == null) return null;
            if (type.IsInstanceOfType(o)) return o;
            object od = DeRef(o);
            if (od != o) return GetValue(type, od);
            return o;
        }

        public override object DeRef(object o)
        {
            if (o is Control)
            {
                Control control = (Control)o;
                if (control.Tag != null) return control.Tag;
                if (!string.IsNullOrEmpty(control.Name)) return control.Name;
                if (!string.IsNullOrEmpty(control.Text)) return control.Text;
            }
            else if (o is ListViewItem)
            {
                ListViewItem control = (ListViewItem)o;
                if (control.Tag != null) return control.Tag;
                if (!string.IsNullOrEmpty(control.Name)) return control.Name;
                if (!string.IsNullOrEmpty(control.Text)) return control.Text;
            }
            return o;
        }

        public object GetValue(Type type, object lastObject)
        {
            if (type == null) return lastObject;
            if (type.IsInstanceOfType(lastObject)) return lastObject;
            if (type.IsAssignableFrom(typeof(Vector2))) return ToUUID(lastObject).ToString();
            if (type.IsAssignableFrom(typeof(SimPosition))) return ToUUID(lastObject).ToString();
            if (type.IsAssignableFrom(typeof(SimObject))) return ToUUID(lastObject).ToString();
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

        public void DebugLog(string s)
        {
            Plugin.DisplayNotificationInChat(string.Format("ContextAction {0}: {1}", Label, s));
        }
    }

    public class CommandContextMenu : CommandContextAction
    {
        readonly HashSet<CommandContextAction> SubCommands = new HashSet<CommandContextAction>();
        override public string ActName
        {
            get
            {
                return Label + "...";
            }
        }

        public override bool Contributes(object o, Type type)
        {
            foreach (var list in SubCommands)
            {
                var its = list.Contributes(o, type);
                if (its) return true;
            }
            return false;
        }

        public CommandContextMenu(RadegastInstance radegastInstance, CogbotRadegastPlugin plugin) : base(radegastInstance,plugin)
        {
            Enabled = true;
        }

        public override IEnumerable<ToolStripMenuItem> GetToolItems(object target, Type type)
        {
            List<ToolStripMenuItem> lst = new List<ToolStripMenuItem>();
            lastObject = target;
            {
                ToolStripMenuItem item = new ToolStripMenuItem(Label + "...", null)
                                             {
                                                 ToolTipText = Label
                                             };
                foreach (var list in SubCommands)
                {
                    var its = list.GetToolItems(target, type);
                    if (its==null) continue;
                    foreach (var menuItem in its)
                    {
                        item.DropDownItems.Add(menuItem);
                        if (!list.Contributes(target, type)) menuItem.Enabled = false;
                    }
                }
                lst.Add(item);
            }
            return ListOfOne("Commands...", lst);
        }
        public override IEnumerable<Control> GetControls(object target, Type type)
        {
            IEnumerable < Control > r = base.GetControls(target, type);
            return r;
        }

        public void AddSubCommand(CommandContextAction command)
        {
            SubCommands.Add(command);
        }
    }
}