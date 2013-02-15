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



public sealed class ExchangeQueryResult : Struct {

    public const int TYPE = 1793;

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
    private String _Type;
    private Dictionary<String,Object> _Arguments;


    public ExchangeQueryResult() {}


    public ExchangeQueryResult(String Type, Dictionary<String,Object> Arguments, params Option[] options) {
        SetType(Type);
        SetArguments(Arguments);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.DURABLE: packing_flags |= 512; break;
            case Option.NOT_FOUND: packing_flags |= 1024; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.ExchangeQueryResult(context, this);
    }


    public bool HasType() {
        return (packing_flags & 256) != 0;
    }

    public ExchangeQueryResult ClearType() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetType() {
        return _Type;
    }

    public ExchangeQueryResult SetType(String value) {
        _Type = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasDurable() {
        return (packing_flags & 512) != 0;
    }

    public ExchangeQueryResult ClearDurable() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public bool GetDurable() {
        return HasDurable();
    }

    public ExchangeQueryResult SetDurable(bool value) {

        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasNotFound() {
        return (packing_flags & 1024) != 0;
    }

    public ExchangeQueryResult ClearNotFound() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public bool GetNotFound() {
        return HasNotFound();
    }

    public ExchangeQueryResult SetNotFound(bool value) {

        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasArguments() {
        return (packing_flags & 2048) != 0;
    }

    public ExchangeQueryResult ClearArguments() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetArguments() {
        return _Arguments;
    }

    public ExchangeQueryResult SetArguments(Dictionary<String,Object> value) {
        _Arguments = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr8(_Type);
        if ((packing_flags & 2048) != 0)
            enc.WriteMap(_Arguments);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Type = dec.ReadStr8();
        if ((packing_flags & 2048) != 0)
            _Arguments = dec.ReadMap();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Type", GetType());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Durable", GetDurable());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_NotFound", GetNotFound());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_Arguments", GetArguments());

			return result;
        }
    }

}
}
