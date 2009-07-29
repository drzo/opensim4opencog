using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Packets;
using OpenMetaverse.StructuredData;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{

    public delegate void OnAddSimObjectCallback(SimObject obj);
    public partial class WorldObjects
    {

        #region OnAddSimObject
        /// <summary>
        /// Triggers the OnNewSimObject event.
        /// </summary>
        public virtual void SendOnAddSimObject(SimObject ea)
        {
            if (OnAddSimObject != null)
                OnAddSimObject(ea);
        }

        public event Action<SimObject> OnAddSimObject;
        #endregion
        
        
        #region OnRemoveSimObject
        /// <summary>
        /// Triggers the OnRemoveSimObject event.
        /// </summary>
        public virtual void SendOnRemoveSimObject(SimObject ea)
        {
            if (OnRemoveSimObject != null)
                OnRemoveSimObject(ea);
        }

        public event Action<SimObject> OnRemoveSimObject;
        #endregion



        #region OnUpdateSimObject
        /// <summary>
        /// Triggers the OnUpdateSimObject event.
        /// </summary>
        public virtual void SendOnUpdateSimObject(SimObject ea, string property, object oldValue, object newValue)
        {
            if (OnUpdateSimObject != null)
                OnUpdateSimObject(ea, property, oldValue, newValue);
        }

        public delegate void OnUpdateSimObjectCallback(SimObject ea, string property, object value, object o);

        public event OnUpdateSimObjectCallback OnUpdateSimObject;
        #endregion



        #region OnAddPartSimObject
        /// <summary>
        /// Triggers the OnAddPartSimObject event.
        /// </summary>
        public virtual void SendOnAddPartSimObject(SimObject ea, string property, object oldValue, object newValue)
        {
            if (OnAddPartSimObject != null)
                OnAddPartSimObject(ea, property, oldValue, newValue);
        }

        public delegate void OnAddPartSimObjectCallback(SimObject ea, string property, object value, object o);

        public event OnAddPartSimObjectCallback OnAddPartSimObject;
        #endregion


        #region OnRemovePartSimObject
        /// <summary>
        /// Triggers the OnRemovePartSimObject event.
        /// </summary>
        public virtual void SendOnRemovePartSimObject(SimObject ea, string property, object oldValue, object newValue)
        {
            if (OnRemovePartSimObject != null)
                OnRemovePartSimObject(ea, property, oldValue, newValue);
        }

        public delegate void OnRemovePartSimObjectCallback(SimObject ea, string property, object value, object o);

        public event OnRemovePartSimObjectCallback OnRemovePartSimObject;
        #endregion



    }
}
