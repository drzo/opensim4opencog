
using Apache.Qpid.Buffer;
using System.Text;

namespace Apache.Qpid.Framing
{
  ///
  /// This class is autogenerated
  /// Do not modify.
  ///
  /// @author Code Generator Script by robert.j.greig@jpmorgan.com
  public class FileReturnBody : AMQMethodBody , IEncodableAMQDataBlock
  {
    public const int CLASS_ID = 70; 	
    public const int METHOD_ID = 70; 	

    public ushort ReplyCode;    
    public string ReplyText;    
    public string Exchange;    
    public string RoutingKey;    
     

    protected override ushort Clazz
    {
        get
        {
            return 70;
        }
    }
   
    protected override ushort Method
    {
        get
        {
            return 70;
        }
    }

    protected override uint BodySize
    {
    get
    {
        
        return (uint)
        2 /*ReplyCode*/+
            (uint)EncodingUtils.EncodedShortStringLength(ReplyText)+
            (uint)EncodingUtils.EncodedShortStringLength(Exchange)+
            (uint)EncodingUtils.EncodedShortStringLength(RoutingKey)		 
        ;
         
    }
    }

    protected override void WriteMethodPayload(ByteBuffer buffer)
    {
        buffer.Put(ReplyCode);
            EncodingUtils.WriteShortStringBytes(buffer, ReplyText);
            EncodingUtils.WriteShortStringBytes(buffer, Exchange);
            EncodingUtils.WriteShortStringBytes(buffer, RoutingKey);
            		 
    }

    protected override void PopulateMethodBodyFromBuffer(ByteBuffer buffer)
    {
        ReplyCode = buffer.GetUInt16();
        ReplyText = EncodingUtils.ReadShortString(buffer);
        Exchange = EncodingUtils.ReadShortString(buffer);
        RoutingKey = EncodingUtils.ReadShortString(buffer);
        		 
    }

    public override string ToString()
    {
        StringBuilder buf = new StringBuilder(base.ToString());
        buf.Append(" ReplyCode: ").Append(ReplyCode);
        buf.Append(" ReplyText: ").Append(ReplyText);
        buf.Append(" Exchange: ").Append(Exchange);
        buf.Append(" RoutingKey: ").Append(RoutingKey);
         
        return buf.ToString();
    }

    public static AMQFrame CreateAMQFrame(ushort channelId, ushort ReplyCode, string ReplyText, string Exchange, string RoutingKey)
    {
        FileReturnBody body = new FileReturnBody();
        body.ReplyCode = ReplyCode;
        body.ReplyText = ReplyText;
        body.Exchange = Exchange;
        body.RoutingKey = RoutingKey;
        		 
        AMQFrame frame = new AMQFrame();
        frame.Channel = channelId;
        frame.BodyFrame = body;
        return frame;
    }
} 
}
