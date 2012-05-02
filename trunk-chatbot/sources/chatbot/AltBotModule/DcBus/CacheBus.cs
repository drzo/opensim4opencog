using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Threading;
using Enyim.Caching;
using Enyim.Caching.Configuration;
using Enyim.Caching.Memcached;
using System.Windows.Forms;
using System.Xml.Serialization;
using System.Xml;
using System.Xml.Schema;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

using System.IO;
using Avro;

using NUnit.Framework;
using Newtonsoft.Json;

namespace Enyim.Caching.Memcached
{
    public  class DCBTranscoder : ITranscoder
    {
        // Its all just strings to me!!!

        const ushort RawDataFlag = 0xfa52;

        public CacheItem Serialize(object o) 
        {
            // - or we just received a byte[]. No further processing is needed.
            if (o is byte[])
            {
                return new CacheItem(RawDataFlag, new ArraySegment<byte>((byte[])o));
            }

            string strdata="";
            strdata = o.ToString();
            if (o is String) { strdata = (string)o;  }
            byte[] data = Encoding.UTF8.GetBytes(strdata);
            return new CacheItem(RawDataFlag, new ArraySegment<byte>((byte[])data));
 
        }

        public object Deserialize(CacheItem item)
        {
            byte[] data = item.Data.Array;
            int offset = item.Data.Offset;
            int count = item.Data.Count;
            string s= Encoding.UTF8.GetString(data, offset, count);
            s = s.Replace("\0", "");
            return s;
        } 

    }
}

namespace DcBus
{
    [Serializable]
    public class CacheBus
    {
        // uses memcached as a communication bus
        MemcachedClientConfiguration m_config = null;
        public MemcachedClient m_client = null;
        public Hashtable m_localHashtable = null;
        public Hashtable m_ourQueues = new Hashtable();
        public Hashtable m_tryCount = new Hashtable();
        ServerStats m_stats = null;
        public int m_try_limit = 8;
        public int m_pulse = 200;
        public Hashtable m_watchers = new Hashtable();
        // Declare a delegate type for processing a book:
        public delegate void processMessageHandler(string message);
        public string _ipAddress = "127.0.0.1";
        [NonSerialized ]
        public System.Windows.Forms.Timer m_clock = null;

        //public processMessageHandler watcher = null;

        public CacheBus()
        {
            try
            {
                m_config = new MemcachedClientConfiguration();
                m_config.Servers.Add(new IPEndPoint(IPAddress.Loopback, 11211));
                m_config.Protocol = MemcachedProtocol.Text;
                m_config.Transcoder = new DCBTranscoder ();

                _ipAddress = IPAddress.Loopback.ToString();

                m_client = new MemcachedClient(m_config);
                m_localHashtable = new Hashtable();
                m_stats = m_client.Stats();

            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public CacheBus(string ipaddress)
        {
            try
            {
                m_config = new MemcachedClientConfiguration();
                m_config.Servers.Add(new IPEndPoint(IPAddress.Parse(ipaddress), 11211));
                m_config.Protocol = MemcachedProtocol.Text;
                m_config.Transcoder = new DCBTranscoder();
                _ipAddress = ipaddress;

                m_client = new MemcachedClient(m_config);
                m_localHashtable = new Hashtable();
                m_stats = m_client.Stats();

            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public void registerWatcher(string queue, processMessageHandler w)
        {
            m_watchers[queue] = w;
        }

        public string getHash0(string Key)
        {
            return (string)m_client.Get<string>(Key);
        }
        public string getHash(string Key)
        {
            string x="";
            Object  buffobj =m_client.Get(Key);
            if (buffobj !=null) x = buffobj.ToString();

            return x;
        }

        public void setHash(string Key, string value)
        {
            string storevalue = value + char.MinValue;
            byte[] buffer = ASCIIEncoding.ASCII.GetBytes(storevalue);
            bool result = m_client.Store(StoreMode.Add, Key, buffer);
            if (result == false)
            {
                m_client.Store(StoreMode.Set, Key, buffer);
            }
        }

        public void startWatch()
        {
            if (m_clock == null)
            {
                m_clock = new System.Windows.Forms.Timer();
                m_clock.Tick += new EventHandler(Clock_tick);
            }
            m_clock.Interval = m_pulse;
            m_clock.Enabled = true;
        }

        public void stopWatch()
        {
            if (m_clock != null)
            {
                m_clock.Enabled = false;
            }
        }

        public ulong getLastReadID(string queue)
        {
            ulong lastID=0;
            if (m_ourQueues.Contains(queue))
            {
                lastID = (ulong)m_ourQueues[queue];
            }
            else
            {
                lastID = 0;
            }
            return lastID;
        }

        public void Clock_tick(object sender, EventArgs eArgs)
        {
            if (m_client == null) return;
            if (sender == m_clock)
            {
                // Check for messages
                Stack STK = new Stack();
                foreach (string ourQueue in m_ourQueues.Keys)
                {
                    STK.Push(ourQueue);
                }

                //foreach (string ourQueue in ourQueues.Keys)
                while (STK.Count > 0)
                {
                    string ourQueue = (string)STK.Pop();
                    ulong headID = m_client.Increment(ourQueue, (ulong)0, (ulong)0);// null increment gets the value
                    ulong lastQid = getLastReadID(ourQueue); // our internal count
                    if (headID == lastQid) continue; // we are up to date

                    processMessageHandler watcher = null;
                    if (m_watchers.ContainsKey(ourQueue)) watcher = (processMessageHandler)m_watchers[ourQueue];
                    while (lastQid != headID)
                    {
                        string message = dequeue(ourQueue);
                        if ((message == "")||(message==null)) break;
                        // notify anybody interested

                        if (watcher != null)
                        {
                            watcher(message);
                        }
                        lastQid = getLastReadID(ourQueue); // our internal count

                    }
                }
            }
        }

        // Normal queues handle one producer, N consumers each keeping an internal 
        // queue pointer to read from

        public void initQueue(string QID)
        {
            if (m_client == null) return;
            // the sender defines the queue
            // we start with slot zero 
            m_client.Store(StoreMode.Set, QID, (ulong)0);
        }

        public void watchQueue(string QID)
        {
            if (m_client == null) return;
            // We want to start watching a queue from zero
            m_ourQueues[QID] = (ulong)0;

        }

        public void watchFreshQueue(string QID)
        {
            // We want to start watching a queue the current head
            m_ourQueues[QID] = m_client.Increment(QID, (ulong)0, (ulong)0); // null increment gets the value
        }

        public void enqueue(string QID, string message)
        {
            if (m_client == null) return;
            if (QID == null) return;

            // Add a new element to the next queue slot
            string tailkey = QID;
            ulong nextID = m_client.Increment(tailkey, (ulong)0, (ulong)1);
            string key = string.Format("{0}:{1}", QID, nextID);
            m_client.Store(StoreMode.Set, key, message);
            m_localHashtable[key] = (string)message;
        }

        public string dequeue(string QID)
        {
            if (m_client == null) return "";
            if (QID == null) return "";

            // are we up to date ?
            ulong headID = m_client.Increment(QID, (ulong)0, (ulong)0); // null increment gets the value
            ulong lastQid = getLastReadID(QID);
            if (headID == lastQid) return "";

            // get the next slot if possible
            lastQid++;
            string key = string.Format("{0}:{1}", QID, lastQid);
            string message = m_client.Get<string>(key);

            // if something then we were successful
            // Note: it may be the case that the writer has incremented
            // but hasnt stored, so you want to skip until next try if nothing is there
            // however he may have died in those 2 instructions and thus cause a dead lock
            // you probably want a maxtries time out

            if (message != null)
            {
                //m_localHashtable[key] = message;
                m_localHashtable[QID] = message;
                if (m_ourQueues.ContainsKey(QID))
                {
                    m_ourQueues[QID] = lastQid;
                    m_tryCount[QID] = (int)0;
                }
            }
            else
            {
                if (m_tryCount.ContainsKey(QID))
                {
                    int tries = (int)m_tryCount[QID];
                    tries++;
                    if (tries > m_try_limit)
                    {
                        // we give up!
                        m_ourQueues[QID] = lastQid;
                        m_tryCount[QID] = (int)0;

                    }
                    else
                    {
                        // keep trying
                        m_tryCount[QID] = (int)tries;
                    }
                }
                else
                {
                    m_tryCount[QID] = (int)0;
                }

            }
            return message;
        }

        // Uniq queues is for times when one consumer per entry is desired

        public Boolean is_empty_uniq(string QID)
        {
            if (m_client == null) return false;
            if (QID == null) return false;
            string headkey = QID + "_head";
            string tailkey = QID + "_tail";
            ulong head = m_client.Increment(headkey, (ulong)0, (ulong)0);
            ulong tail = m_client.Increment(tailkey, (ulong)0, (ulong)0);

            if (head >= tail || head == 0 || tail == 0)
                return true;
            else
                return false;

        }

        public void enqueue_uniq(string QID, string message)
        {
            if (m_client == null) return;
            string headkey = QID + "_head";
            string tailkey = QID + "_tail";
            ulong tail = m_client.Increment(tailkey, (ulong)0, (ulong)1);
            if (tail == 0)
            {
                ulong head = m_client.Increment(headkey, (ulong)0, (ulong)0);

            }
            string key = string.Format("{0}:{1}", QID, tail);
            m_client.Store(StoreMode.Set, key, message);
            m_localHashtable[key] = (string)message;

        }

        public string dequeue_uniq(string QID)
        {
            if (m_client == null) return "";
            string headkey = QID + "_head";
            string tailkey = QID + "_tail";
            string message;

            ulong tail = m_client.Increment(tailkey, (ulong)0, (ulong)0);
            ulong head = m_client.Increment(headkey, (ulong)0, (ulong)1);

            if (head <= tail)
            {
                string key = string.Format("{0}:{1}", QID, head);
                message = m_client.Get<string>(key);

            }
            else
            {
                head = m_client.Decrement(headkey, (ulong)0, (ulong)1);
                message = "";
            }

            return message;
        }

        // N producers and uniq consumers for stacks

        public void pushStack(string SID, string message)
        {
            if (m_client == null) return;
            if (SID == null) return;

            // are we up to date ?
            ulong stkptr = m_client.Increment(SID, (ulong)0, (ulong)1); // null increment gets the value


            string key = string.Format("{0}:{1}", SID, stkptr);
            m_client.Store(StoreMode.Set, key, message);
            m_localHashtable[key] = (string)message;
        }

        public string popStack(string SID)
        {
            if (m_client == null) return "";
            if (SID == null) return "";

            // are we up to date ?
            ulong stkptr = m_client.Decrement(SID, (ulong)0, (ulong)1); // null increment gets the value
            if (stkptr == 0) return "";


            string key = string.Format("{0}:{1}", SID, stkptr);
            string message = m_client.Get<string>(key);
            m_localHashtable[key] = message;
            return message;
        }

        // semaphores
        public bool semLock(string SID)
        {
            if (m_client == null) return false;
            Random rng = new Random();
            string key = "lock:" + SID;
            for (int i = 0; i < 100; i++)
            {
                if (m_client.Store(StoreMode.Add, key, i))
                    return true;
                Thread.Sleep(50 + rng.Next(50));
            }
            return false;
        }

        public bool semUnlock(string SID)
        {
            string key = "lock:" + SID;
            if (m_client == null) return false;
            if (m_client.Remove(key)) return true;
            return false;
        }

        // Serialize whole hash tables
        public HashtableSerailizable getHashTable(string Key)
        {
                HashtableSerailizable htable = null;
                if (m_client != null)
                {
                    //string xml = m_client.Get<string>(Key);
                    object obj = m_client.Get(Key);
                    string xml="";
                    if (obj !=null) xml= obj.ToString();
                    htable = (HashtableSerailizable)MyXmlSerializer.XmlDeserializeFromString(xml, typeof(HashtableSerailizable));
                }
                return htable;
        }

        public void setHashTable(string Key, HashtableSerailizable htable)
        {
            if (m_client == null) return;
            string xml = MyXmlSerializer.XmlSerializeToString(htable);
            m_client.Store(StoreMode.Set, Key, xml);
        }

    }


    /// <summary>
    /// this class is a overload of Hastable that could be automatically sérialized, 
    /// when using System.Xml.Serialization.XmlSerializer.
    /// Thank you to Matt Brether, please visit his site :http://www.mattberther.com/2004/06/14/serializing-an-idictionary-object
    // http://www.codeproject.com/KB/XML/IXmlSerializable.aspx
    /// </summary>
    [Serializable]
    public class HashtableSerailizable : Hashtable, IXmlSerializable
    {


        #region IXmlSerializable Membres

        public System.Xml.Schema.XmlSchema GetSchema()
        {
            return null;
        }

        public void ReadXml(System.Xml.XmlReader reader)
        {
            // Start to use the reader.
            reader.Read();
            // Read the first element ie root of this object
            reader.ReadStartElement("dictionary");

            // Read all elements
            while (reader.NodeType != XmlNodeType.EndElement)
            {
                // parsing the item
                reader.ReadStartElement("item");

                // PArsing the key and value 
                string key = reader.ReadElementString("key");
                //Read the value
                string sType = reader.GetAttribute("type");
                if (sType == null)
                {
                    string value = reader.ReadElementString("value");
                    // add the item
                    this.Add(key, value);
                }
                else
                {
                    reader.ReadStartElement("value");
                    Type valuetype = Type.GetType(sType);
                    XmlSerializer xs = new XmlSerializer(valuetype);
                    this.Add(key, xs.Deserialize(reader));
                    reader.ReadEndElement();
                    reader.ReadEndElement();

                }

                // en reading the item.
                reader.ReadEndElement();
                reader.MoveToContent();

           }

            // Extremely important to read the node to its end.
            // next call of the reader methods will crash if not called.
            reader.ReadEndElement();
        }

        public void WriteXml(System.Xml.XmlWriter writer)
        {
            // Write the root elemnt 
            writer.WriteStartElement("dictionary");

            // Fore each object in this
            foreach (object key in this.Keys)
            {
                object value = this[key];
                // Write item, key and value
                writer.WriteStartElement("item");
                writer.WriteElementString("key", key.ToString());
                //Serialize Value

                Type valuetype = value.GetType();
                if (valuetype.GetInterface("System.Xml.Serialization.IXmlSerializable") == null)
                {
                    writer.WriteElementString("value", value.ToString());
                }
                else
                {
                    XmlSerializer xs = new XmlSerializer(valuetype);
                    writer.WriteStartElement("value");
                    writer.WriteAttributeString("type", valuetype.FullName);
                    xs.Serialize(writer, value);
                    writer.WriteEndElement();
                } 


                // write </item>
                writer.WriteEndElement();
            }
            // write </dictionnary>
            writer.WriteEndElement();
        }
        #endregion
    }

    #region MyXmlSerializer
    public class MyXmlSerializer
    {
        public static void XmlSerialize(XmlWriter writer, object o)
        {
            if (o == null) return;
            XmlSerializer serializer = new XmlSerializer(o.GetType());
            serializer.Serialize(writer, o);
        }

        public static object XmlDeserialize(XmlReader reader, System.Type type)
        {
            XmlSerializer serializer = new XmlSerializer(type);
            return serializer.Deserialize(reader);
        }

        public static string XmlSerializeToString(object o)
        {
            StringBuilder sb = new StringBuilder();
            using (StringWriter stringWriter = new StringWriter(sb))
            {
                XmlWriter xmlWriter = XmlTextWriter.Create(stringWriter);
                try
                {
                    XmlSerialize(xmlWriter, o);
                }
                finally
                {
                    xmlWriter.Close();
                }
            }
            return sb.ToString();
        }

        public static object XmlDeserializeFromString(string xml, System.Type type)
        {
            object returnVal = null;
            if ((xml == null)||(xml.Length==0)) return returnVal;

            using (StringReader reader = new StringReader(xml))
            {
                XmlReader xmlReader = XmlTextReader.Create(reader);
                try
                {
                    XmlSerializer serializer = new XmlSerializer(type);
                    returnVal = serializer.Deserialize(xmlReader);
                }
                finally
                {
                    xmlReader.Close();
                }
            }
            return returnVal;
        }

        public void deepCopy(Hashtable src, out Hashtable dest)
        {
            MemoryStream s = new MemoryStream();
            BinaryFormatter f = new BinaryFormatter();
            f.Serialize(s, src);
            s.Position = 0;
            dest = (Hashtable)f.Deserialize(s);

        }
        public void deepCopy(object src, out object dest)
        {
            
            MemoryStream s = new MemoryStream();
            BinaryFormatter f = new BinaryFormatter();
            f.Serialize(s, src);
            s.Position = 0;
            dest = f.Deserialize(s);
        }

    }
    #endregion


}
