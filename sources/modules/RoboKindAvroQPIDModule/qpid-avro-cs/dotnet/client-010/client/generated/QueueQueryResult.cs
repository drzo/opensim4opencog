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



public sealed class QueueQueryResult : Struct {

    public const int TYPE = 2049;

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
    private String _Queue;
    private String _AlternateExchange;
    private Dictionary<String,Object> _Arguments;
    private long _MessageCount;
    private long _SubscriberCount;


    public QueueQueryResult() {}


    public QueueQueryResult(String Queue, String AlternateExchange, Dictionary<String,Object> Arguments, long MessageCount, long SubscriberCount, params Option[] options) {
        SetQueue(Queue);
        SetAlternateExchange(AlternateExchange);
        SetArguments(Arguments);
        SetMessageCount(MessageCount);
        SetSubscriberCount(SubscriberCount);

        for (int i=0; i < options.Length; i++) {
            switch (options[i]) {
            case Option.DURABLE: packing_flags |= 1024; break;
            case Option.EXCLUSIVE: packing_flags |= 2048; break;
            case Option.AUTO_DELETE: packing_flags |= 4096; break;
            case Option.NONE: break;
            default: throw new Exception("invalid option: " + options[i]);
            }
        }

    }

    public  void Dispatch<C>(C context, MethodDelegate<C> mdelegate) {
        mdelegate.QueueQueryResult(context, this);
    }


    public bool HasQueue() {
        return (packing_flags & 256) != 0;
    }

    public QueueQueryResult ClearQueue() {
        packing_flags = (byte) (packing_flags & ~256);       

        Dirty = true;
        return this;
    }

    public String GetQueue() {
        return _Queue;
    }

    public QueueQueryResult SetQueue(String value) {
        _Queue = value;
        packing_flags |=  256;
        Dirty = true;
        return this;
    }


    public bool HasAlternateExchange() {
        return (packing_flags & 512) != 0;
    }

    public QueueQueryResult ClearAlternateExchange() {
        packing_flags = (byte) (packing_flags & ~512);       

        Dirty = true;
        return this;
    }

    public String GetAlternateExchange() {
        return _AlternateExchange;
    }

    public QueueQueryResult SetAlternateExchange(String value) {
        _AlternateExchange = value;
        packing_flags |=  512;
        Dirty = true;
        return this;
    }


    public bool HasDurable() {
        return (packing_flags & 1024) != 0;
    }

    public QueueQueryResult ClearDurable() {
        packing_flags = (byte) (packing_flags & ~1024);       

        Dirty = true;
        return this;
    }

    public bool GetDurable() {
        return HasDurable();
    }

    public QueueQueryResult SetDurable(bool value) {

        packing_flags |=  1024;
        Dirty = true;
        return this;
    }


    public bool HasExclusive() {
        return (packing_flags & 2048) != 0;
    }

    public QueueQueryResult ClearExclusive() {
        packing_flags = (byte) (packing_flags & ~2048);       

        Dirty = true;
        return this;
    }

    public bool GetExclusive() {
        return HasExclusive();
    }

    public QueueQueryResult SetExclusive(bool value) {

        packing_flags |=  2048;
        Dirty = true;
        return this;
    }


    public bool HasAutoDelete() {
        return (packing_flags & 4096) != 0;
    }

    public QueueQueryResult ClearAutoDelete() {
        packing_flags = (byte) (packing_flags & ~4096);       

        Dirty = true;
        return this;
    }

    public bool GetAutoDelete() {
        return HasAutoDelete();
    }

    public QueueQueryResult SetAutoDelete(bool value) {

        packing_flags |=  4096;
        Dirty = true;
        return this;
    }


    public bool HasArguments() {
        return (packing_flags & 8192) != 0;
    }

    public QueueQueryResult ClearArguments() {
        packing_flags = (byte) (packing_flags & ~8192);       

        Dirty = true;
        return this;
    }

    public Dictionary<String,Object> GetArguments() {
        return _Arguments;
    }

    public QueueQueryResult SetArguments(Dictionary<String,Object> value) {
        _Arguments = value;
        packing_flags |=  8192;
        Dirty = true;
        return this;
    }


    public bool HasMessageCount() {
        return (packing_flags & 16384) != 0;
    }

    public QueueQueryResult ClearMessageCount() {
        packing_flags = (byte) (packing_flags & ~16384);       
        _MessageCount =  0;
        Dirty = true;
        return this;
    }

    public long GetMessageCount() {
        return _MessageCount;
    }

    public QueueQueryResult SetMessageCount(long value) {
        _MessageCount = value;
        packing_flags |=  16384;
        Dirty = true;
        return this;
    }


    public bool HasSubscriberCount() {
        return (packing_flags & 32768) != 0;
    }

    public QueueQueryResult ClearSubscriberCount() {
        packing_flags = (byte) (packing_flags & ~32768);       
        _SubscriberCount =  0;
        Dirty = true;
        return this;
    }

    public long GetSubscriberCount() {
        return _SubscriberCount;
    }

    public QueueQueryResult SetSubscriberCount(long value) {
        _SubscriberCount = value;
        packing_flags |=  32768;
        Dirty = true;
        return this;
    }





    public override void Write(IEncoder enc)
    {
        enc.WriteUint16(packing_flags);
        if ((packing_flags & 256) != 0)
            enc.WriteStr8(_Queue);
        if ((packing_flags & 512) != 0)
            enc.WriteStr8(_AlternateExchange);
        if ((packing_flags & 8192) != 0)
            enc.WriteMap(_Arguments);
        if ((packing_flags & 16384) != 0)
            enc.WriteUint32(_MessageCount);
        if ((packing_flags & 32768) != 0)
            enc.WriteUint32(_SubscriberCount);

    }

    public override void Read(IDecoder dec)
    {
        packing_flags = (int) dec.ReadUint16();
        if ((packing_flags & 256) != 0)
            _Queue = dec.ReadStr8();
        if ((packing_flags & 512) != 0)
            _AlternateExchange = dec.ReadStr8();
        if ((packing_flags & 8192) != 0)
            _Arguments = dec.ReadMap();
        if ((packing_flags & 16384) != 0)
            _MessageCount = dec.ReadUint32();
        if ((packing_flags & 32768) != 0)
            _SubscriberCount = dec.ReadUint32();

    }

    public override Dictionary<String,Object> Fields
    {
		get
		{
			Dictionary<String,Object> result = new Dictionary<String,Object>();

        	if ((packing_flags & 256) != 0)
            	result.Add("_Queue", GetQueue());
        	if ((packing_flags & 512) != 0)
            	result.Add("_AlternateExchange", GetAlternateExchange());
        	if ((packing_flags & 1024) != 0)
            	result.Add("_Durable", GetDurable());
        	if ((packing_flags & 2048) != 0)
            	result.Add("_Exclusive", GetExclusive());
        	if ((packing_flags & 4096) != 0)
            	result.Add("_AutoDelete", GetAutoDelete());
        	if ((packing_flags & 8192) != 0)
            	result.Add("_Arguments", GetArguments());
        	if ((packing_flags & 16384) != 0)
            	result.Add("_MessageCount", GetMessageCount());
        	if ((packing_flags & 32768) != 0)
            	result.Add("_SubscriberCount", GetSubscriberCount());

			return result;
        }
    }

}
}
