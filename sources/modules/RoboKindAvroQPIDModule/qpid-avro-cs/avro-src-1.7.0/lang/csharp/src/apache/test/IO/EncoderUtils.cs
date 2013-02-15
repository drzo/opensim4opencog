using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Apache.Qpid.Messaging;
using Avro.Generic;
using Avro.Specific;
using Avro.Test;

namespace Avro.IO
{
    public class EncoderUtils
    {
        public static IBytesMessage packMessage(object t)
        {
            MemoryStream outs = new MemoryStream();
            Encoder e = new BinaryEncoder(outs);

            var st = new PrimitiveSchema(, null);

            Encode<T> w = new Encode<T>((e0, t0)=>{});

            w(e, t);
            outs.Flush();

            IBytesMessage message = null;
            byte[] data = outs.ToArray();
            message.WriteBytes(data);
            return message;
        }

        public static ISpecificRecord unpackMessage<T>(IBytesMessage message)
        {
            long len = message.BodyLength;
            byte[] data = new byte[(int)len];
            int read = message.ReadBytes(data);
            MemoryStream ins = new MemoryStream(data);

            Avro.IO.Decoder dc = new Avro.IO.BinaryDecoder(ins);

            // Decode<T> r = new Decode<T>();
            // SpecificReader<T> r = new SpecificReader<T>(c);
            SpecificDefaultReader sdr = new SpecificDefaultReader(x, z);
            DatumReader<T> reader = new SpecificReader<T>(sdr);
            T actual = reader.Read(typeof(T), dc);
            //T actual = r(dc);

            ins.Close();
            return (ISpecificRecord) actual;
        }
    }
}
