
using Apache.Qpid.Buffer;
using System.Text;

namespace Apache.Qpid.Framing
{
  ///
  /// This class is autogenerated
  /// Do not modify.
  ///
  /// @author Code Generator Script by robert.j.greig@jpmorgan.com
  public class QueuePurgeBody : AMQMethodBody , IEncodableAMQDataBlock
  {
    public const int CLASS_ID = 50; 	
    public const int METHOD_ID = 30; 	

    public ushort Ticket;    
    public string Queue;    
    public bool Nowait;    
     

    protected override ushort Clazz
    {
        get
        {
            return 50;
        }
    }
   
    protected override ushort Method
    {
        get
        {
            return 30;
        }
    }

    protected override uint BodySize
    {
    get
    {
        
        return (uint)
        2 /*Ticket*/+
            (uint)EncodingUtils.EncodedShortStringLength(Queue)+
            1 /*Nowait*/		 
        ;
         
    }
    }

    protected override void WriteMethodPayload(ByteBuffer buffer)
    {
        buffer.Put(Ticket);
            EncodingUtils.WriteShortStringBytes(buffer, Queue);
            EncodingUtils.WriteBooleans(buffer, new bool[]{Nowait});
            		 
    }

    protected override void PopulateMethodBodyFromBuffer(ByteBuffer buffer)
    {
        Ticket = buffer.GetUInt16();
        Queue = EncodingUtils.ReadShortString(buffer);
        bool[] bools = EncodingUtils.ReadBooleans(buffer);Nowait = bools[0];
        		 
    }

    public override string ToString()
    {
        StringBuilder buf = new StringBuilder(base.ToString());
        buf.Append(" Ticket: ").Append(Ticket);
        buf.Append(" Queue: ").Append(Queue);
        buf.Append(" Nowait: ").Append(Nowait);
         
        return buf.ToString();
    }

    public static AMQFrame CreateAMQFrame(ushort channelId, ushort Ticket, string Queue, bool Nowait)
    {
        QueuePurgeBody body = new QueuePurgeBody();
        body.Ticket = Ticket;
        body.Queue = Queue;
        body.Nowait = Nowait;
        		 
        AMQFrame frame = new AMQFrame();
        frame.Channel = channelId;
        frame.BodyFrame = body;
        return frame;
    }
} 
}
