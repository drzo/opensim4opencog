
using Apache.Qpid.Buffer;
using System.Text;

namespace Apache.Qpid.Framing
{
  ///
  /// This class is autogenerated
  /// Do not modify.
  ///
  /// @author Code Generator Script by robert.j.greig@jpmorgan.com
  public class TestIntegerBody : AMQMethodBody , IEncodableAMQDataBlock
  {
    public const int CLASS_ID = 120; 	
    public const int METHOD_ID = 10; 	

    public byte Integer1;    
    public ushort Integer2;    
    public uint Integer3;    
    public ulong Integer4;    
    public byte Operation;    
     

    protected override ushort Clazz
    {
        get
        {
            return 120;
        }
    }
   
    protected override ushort Method
    {
        get
        {
            return 10;
        }
    }

    protected override uint BodySize
    {
    get
    {
        
        return (uint)
        1 /*Integer1*/+
            2 /*Integer2*/+
            4 /*Integer3*/+
            8 /*Integer4*/+
            1 /*Operation*/		 
        ;
         
    }
    }

    protected override void WriteMethodPayload(ByteBuffer buffer)
    {
        buffer.Put(Integer1);
            buffer.Put(Integer2);
            buffer.Put(Integer3);
            buffer.Put(Integer4);
            buffer.Put(Operation);
            		 
    }

    protected override void PopulateMethodBodyFromBuffer(ByteBuffer buffer)
    {
        Integer1 = buffer.GetByte();
        Integer2 = buffer.GetUInt16();
        Integer3 = buffer.GetUInt32();
        Integer4 = buffer.GetUInt64();
        Operation = buffer.GetByte();
        		 
    }

    public override string ToString()
    {
        StringBuilder buf = new StringBuilder(base.ToString());
        buf.Append(" Integer1: ").Append(Integer1);
        buf.Append(" Integer2: ").Append(Integer2);
        buf.Append(" Integer3: ").Append(Integer3);
        buf.Append(" Integer4: ").Append(Integer4);
        buf.Append(" Operation: ").Append(Operation);
         
        return buf.ToString();
    }

    public static AMQFrame CreateAMQFrame(ushort channelId, byte Integer1, ushort Integer2, uint Integer3, ulong Integer4, byte Operation)
    {
        TestIntegerBody body = new TestIntegerBody();
        body.Integer1 = Integer1;
        body.Integer2 = Integer2;
        body.Integer3 = Integer3;
        body.Integer4 = Integer4;
        body.Operation = Operation;
        		 
        AMQFrame frame = new AMQFrame();
        frame.Channel = channelId;
        frame.BodyFrame = body;
        return frame;
    }
} 
}
