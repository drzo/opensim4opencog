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



public sealed class MessageReject : Method {

    public const int TYPE = 1027;

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
    private RangeSet _Transfers;
    private MessageRejectCode _Code;
    private String _Text;


    public MessageReject() {}


    public MessageReject(RangeSet Transfers, MessageRejectCode Code, String Text, params Option[] options) {
        SetTransfers(Transfers);
        SetCode(Code);
        SetText(Text);

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
        mdelegate.MessageReject(context, this);
    }


    public bool HasTransfers() {
        return (packing_flags & 256) != 0;
    }

    public MessageReject ClearTransfers() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public RangeSet GetTransfers() {
        return _Transfers;
    }

    public MessageReject SetTransfers(RangeSet value) {
        _Transfers = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasCode() {
        return (packing_flags & 512) != 0;
    }

    public MessageReject ClearCode() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public MessageRejectCode GetCode() {
        return _Code;
    }

    public MessageReject SetCode(MessageRejectCode value) {
        _Code = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasText() {
        return (packing_flags & 1024) != 0;
    }

    public MessageReject ClearText() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public String GetText() {
        return _Text;
    }

    public MessageReject SetText(String value) {
        _Text = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteSequenceSet(_Transfers);
        if ((packing_flags & 512) != 0)
            enc.WriteUint16((short)_Code);
        if ((packing_flags & 1024) != 0)
            enc.WriteStr8(_Text);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Transfers = dec.ReadSequenceSet();
        if ((packing_flags & 512) != 0)
            _Code = MessageRejectCodeGetter.Get(dec.ReadUint16());
        if ((packing_flags & 1024) != 0)
            _Text = dec.ReadStr8();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Transfers", GetTransfers());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Code", GetCode());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_Text", GetText());

			return result;
        }
    }

}
}
