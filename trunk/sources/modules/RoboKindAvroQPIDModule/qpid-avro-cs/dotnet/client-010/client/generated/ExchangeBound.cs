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



public sealed class ExchangeBound : Method {

    public const int TYPE = 1798;

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
    private String _Exchange;
    private String _Queue;
    private String _BindingKey;
    private Dictionary<String,Object> _Arguments;


    public ExchangeBound() {}


    public ExchangeBound(String Exchange, String Queue, String BindingKey, Dictionary<String,Object> Arguments, params Option[] options) {
        SetExchange(Exchange);
        SetQueue(Queue);
        SetBindingKey(BindingKey);
        SetArguments(Arguments);

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
        mdelegate.ExchangeBound(context, this);
    }


    public bool HasExchange() {
        return (packing_flags & 256) != 0;
    }

    public ExchangeBound ClearExchange() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetExchange() {
        return _Exchange;
    }

    public ExchangeBound SetExchange(String value) {
        _Exchange = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasQueue() {
        return (packing_flags & 512) != 0;
    }

    public ExchangeBound ClearQueue() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public String GetQueue() {
        return _Queue;
    }

    public ExchangeBound SetQueue(String value) {
        _Queue = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasBindingKey() {
        return (packing_flags & 1024) != 0;
    }

    public ExchangeBound ClearBindingKey() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public String GetBindingKey() {
        return _BindingKey;
    }

    public ExchangeBound SetBindingKey(String value) {
        _BindingKey = value;
        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasArguments() {
        return (packing_flags & 2048) != 0;
    }

    public ExchangeBound ClearArguments() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetArguments() {
        return _Arguments;
    }

    public ExchangeBound SetArguments(Dictionary<String,Object> value) {
        _Arguments = value;
        packing_flags |=  2048;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr8(_Exchange);
        if ((packing_flags & 512) != 0)
            enc.WriteStr8(_Queue);
        if ((packing_flags & 1024) != 0)
            enc.WriteStr8(_BindingKey);
        if ((packing_flags & 2048) != 0)
            enc.WriteMap(_Arguments);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Exchange = dec.ReadStr8();
        if ((packing_flags & 512) != 0)
            _Queue = dec.ReadStr8();
        if ((packing_flags & 1024) != 0)
            _BindingKey = dec.ReadStr8();
        if ((packing_flags & 2048) != 0)
            _Arguments = dec.ReadMap();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Exchange", GetExchange());
        	if ((packing_flags & 512) != 0)
            	result.Add("_Queue", GetQueue());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_BindingKey", GetBindingKey());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_Arguments", GetArguments());

			return result;
        }
    }

}
}
