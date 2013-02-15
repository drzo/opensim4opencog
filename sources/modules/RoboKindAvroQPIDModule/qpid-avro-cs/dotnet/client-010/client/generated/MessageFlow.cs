/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

using System;
using org.apache.qpid.transport.codec;
using System.Collections.Generic;
using org.apache.qpid.transport.util;
using org.apache.qpid.transport.network;
using System.IO;

namespace org.apache.qpid.transport
{



public sealed class MessageFlow : Method {

    public const int TYPE = 1034;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 0;
    }

    public override int GetPackWidth() {
        return 2;
    }

    public override bool HasPayload() {
        return false;
    }

    public override byte EncodedTrack 
    {
       get{ return Frame.L4; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private String _Destination;
    private MessageCreditUnit _Unit;
    private long _Value;


    public MessageFlow() {}


    public MessageFlow(String Destination, MessageCreditUnit Unit, long Value, params Option[] options) {
        SetDestination(Destination);
        SetUnit(Unit);
        SetValue(Value);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.SYNC: Sync = true; break;
            case Option.BATCH: Batch = true; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public override void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.MessageFlow(context, this);
    }


    public bool HasDestination() {
        return (packing_flags & 256) != 0;
    }

    public MessageFlow ClearDestination() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetDestination() {
        return _Destination;
    }

    public MessageFlow SetDestination(String value) {
        _Destination = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasUnit() {
        return (packing_flags & 512) != 0;
    }

    public MessageFlow ClearUnit() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public MessageCreditUnit GetUnit() {
        return _Unit;
    }

    public MessageFlow SetUnit(MessageCreditUnit value) {
        _Unit = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasValue() {
        return (packing_flags & 1024) != 0;
    }

    public MessageFlow ClearValue() {
        packing_flags = (byte) (packing_flags & ~1024);       
        _Value =  0;
        Dirty = true;
        return this;
    }

    public long GetValue() {
        return _Value;
    }

    public MessageFlow SetValue(long value) {
        _Value = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr8(_Destination);
        if ((packing_flags & 512) != 0)
            enc.WriteUint8((short)_Unit);
        if ((packing_flags & 1024) != 0)
            enc.WriteUint32(_Value);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Destination = dec.ReadStr8();
        if ((packing_flags & 512) != 0)
            _Unit = MessageCreditUnitGetter.Get(dec.ReadUint8());
        if ((packing_flags & 1024) != 0)
            _Value = dec.ReadUint32();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Destination", GetDestination());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Unit", GetUnit());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_Value", GetValue());

			return result;
        }
    }

}
}
