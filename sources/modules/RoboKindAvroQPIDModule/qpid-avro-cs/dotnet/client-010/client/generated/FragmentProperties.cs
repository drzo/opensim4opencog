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



public sealed class FragmentProperties : Struct {

    public const int TYPE = 1026;

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
    private long _FragmentSize;


    public FragmentProperties() {}


    public FragmentProperties(long FragmentSize, params Option[] options) {
        SetFragmentSize(FragmentSize);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.FIRST: packing_flags |= 256; break;
            case Option.LAST: packing_flags |= 512; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.FragmentProperties(context, this);
    }


    public bool HasFirst() {
        return (packing_flags & 256) != 0;
    }

    public FragmentProperties ClearFirst() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public bool GetFirst() {
        return HasFirst();
    }

    public FragmentProperties SetFirst(bool value) {

        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasLast() {
        return (packing_flags & 512) != 0;
    }

    public FragmentProperties ClearLast() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public bool GetLast() {
        return HasLast();
    }

    public FragmentProperties SetLast(bool value) {

        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasFragmentSize() {
        return (packing_flags & 1024) != 0;
    }

    public FragmentProperties ClearFragmentSize() {
        packing_flags = (byte) (packing_flags & ~1024);       
        _FragmentSize =  0;
        Dirty = true;
        return this;
    }

    public long GetFragmentSize() {
        return _FragmentSize;
    }

    public FragmentProperties SetFragmentSize(long value) {
        _FragmentSize = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 1024) != 0)
            enc.WriteUint64(_FragmentSize);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 1024) != 0)
            _FragmentSize = dec.ReadUint64();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_First", GetFirst());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Last", GetLast());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_FragmentSize", GetFragmentSize());

			return result;
        }
    }

}
}
