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



public sealed class Xid : Struct {

    public const int TYPE = 1540;

    public override int GetStructType() {
        return TYPE;
    }

    public override int GetSizeWidth() {
        return 4;
    }

    public override int GetPackWidth() {
        return 2;
    }

    public  bool HasPayload() {
        return false;
    }

    public  byte EncodedTrack 
    {
       get{ return 4; }
       set { throw new NotImplementedException(); }
    }

    private int packing_flags = 0;
    private long _Format;
    private byte[] _GlobalId;
    private byte[] _BranchId;


    public Xid() {}


    public Xid(long Format, byte[] GlobalId, byte[] BranchId) {
        SetFormat(Format);
        SetGlobalId(GlobalId);
        SetBranchId(BranchId);

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.Xid(context, this);
    }


    public bool HasFormat() {
        return (packing_flags & 256) != 0;
    }

    public Xid ClearFormat() {
        packing_flags = (byte) (packing_flags & ~256);       
        _Format =  0;
        Dirty = true;
        return this;
    }

    public long GetFormat() {
        return _Format;
    }

    public Xid SetFormat(long value) {
        _Format = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasGlobalId() {
        return (packing_flags & 512) != 0;
    }

    public Xid ClearGlobalId() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public byte[] GetGlobalId() {
        return _GlobalId;
    }

    public Xid SetGlobalId(byte[] value) {
        _GlobalId = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasBranchId() {
        return (packing_flags & 1024) != 0;
    }

    public Xid ClearBranchId() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public byte[] GetBranchId() {
        return _BranchId;
    }

    public Xid SetBranchId(byte[] value) {
        _BranchId = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteUint32(_Format);
        if ((packing_flags & 512) != 0)
            enc.WriteVbin8(_GlobalId);
        if ((packing_flags & 1024) != 0)
            enc.WriteVbin8(_BranchId);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Format = dec.ReadUint32();
        if ((packing_flags & 512) != 0)
            _GlobalId = dec.ReadVbin8();
        if ((packing_flags & 1024) != 0)
            _BranchId = dec.ReadVbin8();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Format", GetFormat());
        	if ((packing_flags & 512) != 0)
            	result.Add("_GlobalId", GetGlobalId());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_BranchId", GetBranchId());

			return result;
        }
    }

}
}
