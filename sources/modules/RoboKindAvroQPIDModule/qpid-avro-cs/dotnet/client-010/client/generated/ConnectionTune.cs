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



public sealed class ConnectionTune : Method {

    public const int TYPE = 261;

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
       get{ return Frame.L1; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private int _ChannelMax;
    private int _MaxFrameSize;
    private int _HeartbeatMin;
    private int _HeartbeatMax;


    public ConnectionTune() {}


    public ConnectionTune(int ChannelMax, int MaxFrameSize, int HeartbeatMin, int HeartbeatMax, params Option[] options) {
        SetChannelMax(ChannelMax);
        SetMaxFrameSize(MaxFrameSize);
        SetHeartbeatMin(HeartbeatMin);
        SetHeartbeatMax(HeartbeatMax);

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
        mdelegate.ConnectionTune(context, this);
    }


    public bool HasChannelMax() {
        return (packing_flags & 256) != 0;
    }

    public ConnectionTune ClearChannelMax() {
        packing_flags = (byte) (packing_flags & ~256);       
        _ChannelMax =  0;
        Dirty = true;
        return this;
    }

    public int GetChannelMax() {
        return _ChannelMax;
    }

    public ConnectionTune SetChannelMax(int value) {
        _ChannelMax = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasMaxFrameSize() {
        return (packing_flags & 512) != 0;
    }

    public ConnectionTune ClearMaxFrameSize() {
        packing_flags = (byte) (packing_flags & ~512);       
        _MaxFrameSize =  0;
        Dirty = true;
        return this;
    }

    public int GetMaxFrameSize() {
        return _MaxFrameSize;
    }

    public ConnectionTune SetMaxFrameSize(int value) {
        _MaxFrameSize = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasHeartbeatMin() {
        return (packing_flags & 1024) != 0;
    }

    public ConnectionTune ClearHeartbeatMin() {
        packing_flags = (byte) (packing_flags & ~1024);       
        _HeartbeatMin =  0;
        Dirty = true;
        return this;
    }

    public int GetHeartbeatMin() {
        return _HeartbeatMin;
    }

    public ConnectionTune SetHeartbeatMin(int value) {
        _HeartbeatMin = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasHeartbeatMax() {
        return (packing_flags & 2048) != 0;
    }

    public ConnectionTune ClearHeartbeatMax() {
        packing_flags = (byte) (packing_flags & ~2048);       
        _HeartbeatMax =  0;
        Dirty = true;
        return this;
    }

    public int GetHeartbeatMax() {
        return _HeartbeatMax;
    }

    public ConnectionTune SetHeartbeatMax(int value) {
        _HeartbeatMax = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteUint16(_ChannelMax);
        if ((packing_flags & 512) != 0)
            enc.WriteUint16(_MaxFrameSize);
        if ((packing_flags & 1024) != 0)
            enc.WriteUint16(_HeartbeatMin);
        if ((packing_flags & 2048) != 0)
            enc.WriteUint16(_HeartbeatMax);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _ChannelMax = dec.ReadUint16();
        if ((packing_flags & 512) != 0)
            _MaxFrameSize = dec.ReadUint16();
        if ((packing_flags & 1024) != 0)
            _HeartbeatMin = dec.ReadUint16();
        if ((packing_flags & 2048) != 0)
            _HeartbeatMax = dec.ReadUint16();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_ChannelMax", GetChannelMax());
        	if ((packing_flags & 512) != 0)
            	result.Add("_MaxFrameSize", GetMaxFrameSize());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_HeartbeatMin", GetHeartbeatMin());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_HeartbeatMax", GetHeartbeatMax());

			return result;
        }
    }

}
}
