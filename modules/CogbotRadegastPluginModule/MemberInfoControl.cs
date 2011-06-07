using System;
using System.Collections.Generic;
using System.Drawing;
using System.Reflection;
using System.Windows.Forms;
using OpenMetaverse;

namespace CogbotRadegastPluginModule
{
    public class MemberInfoControl
    {
        public MemberInfo info;
        private object _target;
        private bool _isReadOnly;
        readonly Control _control;

        public object Target
        {
            get { return _target; }
            set { _target = value; }
        }

        public bool IsReadOnly
        {
            get
            {
                if (ValueChanged == null) return true;
                if (!CanEdit(Target)) return true;
                return _isReadOnly;
            }
            set
            {
                _isReadOnly = value;
                SetChildrenEnabled(Control, value);
            }
        }

        public Control Control
        {
            get { return _control; }
        }

        public Control.ControlCollection Controls
        {
            get { return _control.Controls; }
        }

        private void SetChildrenEnabled(Control control, bool value)
        {
            bool baren = true;
            foreach (Control c in Control.Controls)
            {
                c.Enabled = value;
                baren = false;
            }
            if (baren) control.Enabled = value;
        }

        private bool CanEdit(object target)
        {
            if (_isReadOnly) return false;
            if (info == null) return true;
            return false;
        }

        #region OnValueChanged
        public event Action<object> ValueChanged;/// <summary>
        /// Triggers the ValueChanged event.
        /// </summary>
        public virtual void OnValueChanged(object ea)
        {
            if (ValueChanged != null)
                ValueChanged(ea);
        }
        #endregion

        public MemberInfoControl(Control info)
        {
            this._control = info;
        }

        public static MemberInfoControl GetPropertyController(FieldInfo info)
        {
            if (info == null) return null;
            MemberInfoControl mic = GetPropertyController(info.Name, info.FieldType);
            if (mic == null) return null;
            mic.info = info;
            mic.ValueChanged += new Action<object>(o =>
                                                       {
                                                           info.SetValue(mic.Target, o);
                                                       });
            Control outer = mic.Control;
            outer.Name = string.Format("lbl_{0}_{1}", info.DeclaringType.Name, info.Name);
            outer.Text = info.Name;
            //if (!TryMakeControl(info, outer)) return null;
            return mic;
        }

        public static MemberInfoControl GetPropertyController(MemberInfo info)
        {
            return GetPropertyController(info as PropertyInfo) ?? GetPropertyController(info as FieldInfo);
        }

        public static MemberInfoControl GetPropertyController(PropertyInfo info)
        {
            if (info == null) return null;
            if (!info.CanRead) return null;
            MemberInfoControl mic = GetPropertyController(info.Name, info.PropertyType);
            if (mic == null) return null;
            mic.info = info;
            mic.ValueChanged += new Action<object>(o =>
            {
                info.SetValue(mic.Target, o, null);
            });
            Control outer = mic.Control;
            outer.Name = string.Format("lbl_{0}_{1}", info.DeclaringType.Name, info.Name);
            outer.Text = info.Name;
            mic.IsReadOnly = !info.CanWrite;
            //if (!TryMakeControl(info, outer)) return null;
            return mic;
        }


        private static bool TryMakeControl(FieldInfo fieldInfo, GroupBox type)
        {
            return false;
        }

        public static MemberInfoControl GetPropertyController(string name, Type ftype)
        {
            if (IsBitFlags(ftype))
            {
                var fs = ftype.GetFields(BindingFlags.Static);
                int total = fs.Length;
                int len = total / 4;
                Size size = new System.Drawing.Size(15 * total + 60, len * 39 + 39);
                FlowLayoutPanel flow = new FlowLayoutPanel();
                flow.FlowDirection = FlowDirection.LeftToRight;
                flow.AutoSize = false;
                flow.Size = size;
                flow.Dock = DockStyle.Bottom;
                flow.MinimumSize = size;

                foreach (var o in fs)
                {
                    CheckBox inner = new CheckBox();
                    inner.Size = new System.Drawing.Size(54, 21);
                    inner.Text = o.Name;
                    inner.Name = string.Format("value_{0}_{1}", name, o.Name);
                    inner.KeyUp += (delegate(object sender, KeyEventArgs e)
                    {
                    });
                    flow.Controls.Add(inner);
                }
                Control outer = new GroupBox();
                outer.Size = size;
                outer.MinimumSize = size + new Size(5, 5);
                outer.Controls.Add(flow);
                outer.AutoSize = false;
                MemberInfoControl mic = new MemberInfoControl(outer);
                return mic;
            }
            if (ftype.IsEnum)
            {
                ComboBox inner = new ComboBox();
                inner.FormattingEnabled = true;
                inner.Dock = DockStyle.Bottom;
                inner.Name = "value_" + name;
                foreach (FieldInfo o in ftype.GetFields())
                {
                    if (o.IsStatic)
                    {
                        inner.Items.Add(o.Name);
                    }
                }
                inner.SelectedIndex = 0;
                inner.SelectedIndexChanged += new System.EventHandler(delegate(object sender, EventArgs e)
                                                                                {

                                                                                });
                Control outer = new GroupBox();
                outer.Size = new System.Drawing.Size(149, 39);
                outer.Controls.Add(inner);
                MemberInfoControl mic = new MemberInfoControl(outer);
                return mic;
            }

            if (ftype == typeof(String))
            {
                TextBox inner = new TextBox();
                inner.Dock = DockStyle.Bottom;
                inner.Name = "value_" + name;
                inner.KeyUp += (delegate(object sender, KeyEventArgs e)
                                          {

                                          });
                Control outer = new GroupBox();
                outer.Size = new System.Drawing.Size(149, 39);
                outer.Controls.Add(inner);
                MemberInfoControl mic = new MemberInfoControl(outer);
                return mic;
            }

            if (typeof(bool).IsAssignableFrom(ftype))
            {
                CheckBox inner = new CheckBox();
                inner.Dock = DockStyle.Bottom;
                inner.Name = "value_" + name;
                inner.Text = name;
                inner.Size = new System.Drawing.Size(90, 39);
                GetResult<object> act = new GetResult<object>();
                inner.KeyUp += (delegate(object sender, KeyEventArgs e)
                {
                    if (e.KeyCode == Keys.Enter)
                    {
                        // mic.OnValueChanged(act);
                    }
                });
                MemberInfoControl mic = new MemberInfoControl(inner);
                return mic;
            }

            if (IsNumber(ftype))
            {
                TextBox inner = new TextBox();
                inner.Dock = DockStyle.Bottom;
                inner.Name = "value_" + name;
                inner.Size = new System.Drawing.Size(50, 21);
                GetResult<object> act = new GetResult<object>();
                inner.KeyUp += (delegate(object sender, KeyEventArgs e)
                                          {
                                              if (e.KeyCode == Keys.Enter)
                                              {
                                                  // mic.OnValueChanged(act);
                                              }
                                          });
                Control outer = new GroupBox();
                outer.Size = new System.Drawing.Size(90, 39);
                outer.Controls.Add(inner);
                MemberInfoControl mic = new MemberInfoControl(outer);
                return mic;

            }

            if (IsNumberList(ftype,5))
            {
                FlowLayoutPanel flow = new FlowLayoutPanel();
                flow.Dock = DockStyle.Bottom;
                var fs = ftype.GetFields(BindingFlags.Instance | BindingFlags.Public);
                flow.FlowDirection = FlowDirection.LeftToRight;
                Size size = new System.Drawing.Size(60*fs.Length, 39);
                flow.MinimumSize = size;
                Label label = new Label();
                label.Text = ftype.Name;
                foreach (var o in fs)
                {
                    string oName = o.Name;
                    //labelText += oName.Length > 1 ? oName.Substring(oName.Length - 1) : oName;
                }
                label.Size = new System.Drawing.Size(20, 13);
                label.Name = string.Format("lbl_{0}", name);
                TextBox inner = new TextBox();
                inner.Size = new System.Drawing.Size(60, 21);
                inner.Name = string.Format("value_{0}", name);
                inner.KeyUp += (delegate(object sender, KeyEventArgs e)
                {
                });
                flow.Controls.Add(label);
                flow.Controls.Add(inner);
                Control outer = new GroupBox();
                outer.MinimumSize = size + new Size(5, 10);
                outer.Size = size + new Size(5, 10);
                flow.Size = size;
                outer.Controls.Add(flow);
                MemberInfoControl mic = new MemberInfoControl(outer);
                return mic;
            }
            return null;
        }

        static System.Collections.IEnumerable Unfold(object value)
        {
            IList<object> results = new List<object>();
            var type = value.GetType();
            var utype = Enum.GetUnderlyingType(type);
            var values = Enum.GetValues(type);
            if (utype == typeof(byte) || utype == typeof(sbyte) || utype == typeof(Int16) || utype == typeof(UInt16) || utype == typeof(Int32))
            {
                var num = (Int32)Convert.ChangeType(value, typeof(Int32));
                foreach (var val in values)
                {
                    var v = (Int32)Convert.ChangeType(val, typeof(Int32));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(UInt32))
            {
                var num = (UInt32)value;
                foreach (var val in values)
                {
                    var v = (UInt32)Convert.ChangeType(val, typeof(UInt32));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(Int64))
            {
                var num = (Int64)value;
                foreach (var val in values)
                {
                    var v = (Int64)Convert.ChangeType(val, typeof(Int64));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(UInt64))
            {
                var num = (UInt64)value;
                foreach (var val in values)
                {
                    var v = (UInt64)Convert.ChangeType(val, typeof(UInt64));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else
            {
                throw new NotSupportedException();
            }
            return results;
        }

        private delegate void WithEnum(Enum p);
        private void ForEachEnumValue(WithEnum withValue, object p)
        {
            Type pType = p.GetType();
            Array pTypeValues = System.Enum.GetValues(pType);
            if (p is byte)
            {
                byte b = (byte)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    byte bv = (byte)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is sbyte)
            {
                sbyte b = (sbyte)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    sbyte bv = (sbyte)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt16)
            {
                ushort b = (UInt16)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    ushort bv = (ushort)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int16)
            {
                short b = (Int16)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    short bv = (short)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt32)
            {
                uint b = (UInt32)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    uint bv = (uint)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int32)
            {
                int b = (Int32)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    int bv = (int)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt64)
            {
                ulong b = (UInt64)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    ulong bv = (ulong)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int64)
            {
                long b = (Int64)p;
                if (b == 0)
                {
                    withValue((Enum)(p));
                }
                foreach (object v in pTypeValues)
                {
                    long bv = (long)v;
                    if (bv >= b)
                    {
                        withValue((Enum)(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            string s = p.ToString();
            foreach (var unfold in Unfold(p))
            {
                withValue((Enum)(unfold));
                return;
            }

            if (p is IConvertible)
            {
                withValue((Enum)(p));
                return;
            }

            if (p is Enum)
            {
                withValue((Enum)(p));
                return;
            }
            withValue((Enum)(p));
        }


        private static bool IsBitFlags(Type ftype)
        {
            if (ftype.IsEnum)
            {
                foreach (var e in Enum.GetValues(ftype))
                {
                    int i = e.GetHashCode();
                    if (i <= 1) continue;
                    if ((i & (i - 1)) != 0) return false;
                }
                return true;
            }
            return false;
        }

        static bool IsNumberList(Type ftype, int maxFields)
        {
            int found = 0;
            foreach (var o in ftype.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                if (!IsNumber(o.FieldType))
                {
                    return false;
                }
                found++;
                if (found > maxFields) return false;
            }
            return found > 0;
        }

        private static bool IsNumber(Type o)
        {
            return typeof(IConvertible).IsAssignableFrom(o) && typeof(IFormattable).IsAssignableFrom(o);
        }
    }

    public class GetResult<T>
    {
    }
}