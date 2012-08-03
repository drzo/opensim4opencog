
using Apache.Qpid.Buffer;
using System.Text;

namespace Apache.Qpid.Framing
{
  ///
  /// This class is autogenerated
  /// Do not modify.
  ///
  /// @author Code Generator Script by robert.j.greig@jpmorgan.com
  public class DtxSelectOkBody : AMQMethodBody , IEncodableAMQDataBlock
  {
    public const int CLASS_ID = 100; 	
    public const int METHOD_ID = 11; 	

     

    protected override ushort Clazz
    {
        get
        {
            return 100;
        }
    }
   
    protected override ushort Method
    {
        get
        {
            return 11;
        }
    }

    protected override uint BodySize
    {
    get
    {
        return 0; 
    }
    }

    protected override void WriteMethodPayload(ByteBuffer buffer)
    {
        		 
    }

    protected override void PopulateMethodBodyFromBuffer(ByteBuffer buffer)
    {
        		 
    }

    public override string ToString()
    {
        StringBuilder buf = new StringBuilder(base.ToString());
         
        return buf.ToString();
    }

    public static AMQFrame CreateAMQFrame(ushort channelId)
    {
        DtxSelectOkBody body = new DtxSelectOkBody();
        		 
        AMQFrame frame = new AMQFrame();
        frame.Channel = channelId;
        frame.BodyFrame = body;
        return frame;
    }
} 
}
