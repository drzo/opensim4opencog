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



public sealed class SessionCommandFragment : Struct {

    public const int TYPE = -2;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 0;
    }

    public override int GetPackWidth() {
        return 0;
    }

    public  bool HasPayload() {
        return false;
    }

    public  byte EncodedTrack 
    {
       get{ return 4; }
       set { throw new NotImplementedException(); }
    }

    private int _CommandId;
    private RangeSet _ByteRanges;


    public SessionCommandFragment() {}


    public SessionCommandFragment(int CommandId, RangeSet ByteRanges) {
        SetCommandId(CommandId);
        SetByteRanges(ByteRanges);

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.SessionCommandFragment(context, this);
    }


    public int GetCommandId() {
        return _CommandId;
    }

    public SessionCommandFragment SetCommandId(int value) {
        _CommandId = value;

        Dirty = true;
        return this;
    }


    public RangeSet GetByteRanges() {
        return _ByteRanges;
    }

    public SessionCommandFragment SetByteRanges(RangeSet value) {
        _ByteRanges = value;

        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteSequenceNo(_CommandId);
        enc.WriteByteRanges(_ByteRanges);

    }

    public override void Read(IDecoder dec)
    {
        _CommandId = dec.ReadSequenceNo();
        _ByteRanges = dec.ReadByteRanges();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	result.Add("_CommandId", GetCommandId());
        	result.Add("_ByteRanges", GetByteRanges());

			return result;
        }
    }

}
}
