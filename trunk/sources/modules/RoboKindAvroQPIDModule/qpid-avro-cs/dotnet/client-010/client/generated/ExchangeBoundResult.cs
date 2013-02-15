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



public sealed class ExchangeBoundResult : Struct {

    public const int TYPE = 1794;

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


    public ExchangeBoundResult() {}


    public ExchangeBoundResult(params Option[] options) {

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.EXCHANGE_NOT_FOUND: packing_flags |= 256; break;
            case Option.QUEUE_NOT_FOUND: packing_flags |= 512; break;
            case Option.QUEUE_NOT_MATCHED: packing_flags |= 1024; break;
            case Option.KEY_NOT_MATCHED: packing_flags |= 2048; break;
            case Option.ARGS_NOT_MATCHED: packing_flags |= 4096; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.ExchangeBoundResult(context, this);
    }


    public bool HasExchangeNotFound() {
        return (packing_flags & 256) != 0;
    }

    public ExchangeBoundResult ClearExchangeNotFound() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public bool GetExchangeNotFound() {
        return HasExchangeNotFound();
    }

    public ExchangeBoundResult SetExchangeNotFound(bool value) {

        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasQueueNotFound() {
        return (packing_flags & 512) != 0;
    }

    public ExchangeBoundResult ClearQueueNotFound() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public bool GetQueueNotFound() {
        return HasQueueNotFound();
    }

    public ExchangeBoundResult SetQueueNotFound(bool value) {

        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasQueueNotMatched() {
        return (packing_flags & 1024) != 0;
    }

    public ExchangeBoundResult ClearQueueNotMatched() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public bool GetQueueNotMatched() {
        return HasQueueNotMatched();
    }

    public ExchangeBoundResult SetQueueNotMatched(bool value) {

        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasKeyNotMatched() {
        return (packing_flags & 2048) != 0;
    }

    public ExchangeBoundResult ClearKeyNotMatched() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public bool GetKeyNotMatched() {
        return HasKeyNotMatched();
    }

    public ExchangeBoundResult SetKeyNotMatched(bool value) {

        packing_flags |=  2048;
        Dirty = true;
        return this;
    }


    public bool HasArgsNotMatched() {
        return (packing_flags & 4096) != 0;
    }

    public ExchangeBoundResult ClearArgsNotMatched() {
        packing_flags = (byte) (packing_flags & ~4096);       

        Dirty = true;
        return this;
    }

    public bool GetArgsNotMatched() {
        return HasArgsNotMatched();
    }

    public ExchangeBoundResult SetArgsNotMatched(bool value) {

        packing_flags |=  4096;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_ExchangeNotFound", GetExchangeNotFound());
        	if ((packing_flags & 512) != 0)
            	result.Add("_QueueNotFound", GetQueueNotFound());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_QueueNotMatched", GetQueueNotMatched());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_KeyNotMatched", GetKeyNotMatched());
        	if ((packing_flags & 4096) != 0)
            	result.Add("_ArgsNotMatched", GetArgsNotMatched());

			return result;
        }
    }

}
}
